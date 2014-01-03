{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Marquise
(
    -- *Exceptions
    -- |
    -- This library should only ever throw a 'MarquiseError'.
    E.MarquiseError(..),
    -- *Marquise monad
    runMarquise,
    -- *Sending functions
    sendText,
    sendInt,
    sendReal,
    sendCounter,
    sendBinary,
    -- *Convenience
    timeNow,
) where

import qualified System.Marquise.Error as E
import qualified System.Marquise.FFI as F

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as SV
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types
import System.Clock

newtype Marquise a = Marquise (ReaderT (Ptr F.MarquiseConnection) IO a)
    deriving (Monad, MonadIO, MonadReader (Ptr F.MarquiseConnection))

withConsumer :: B.ByteString
             -> Rational
             -> (Ptr F.MarquiseConsumer -> IO a)
             -> IO a
withConsumer broker poll f =
    B.useAsCString broker $ \broker_cstr -> do
        consumer <- E.throwIfNull "marquise_consumer_new" $
            F.c_marquise_consumer_new broker_cstr (fromRational poll)
        result <- f consumer
        F.c_marquise_consumer_shutdown consumer
        return result

withConnection :: Ptr F.MarquiseConsumer
               -> (Ptr F.MarquiseConnection -> IO a)
               -> IO a
withConnection consumer f = do
    connection <- E.throwIfNull "marquise_connect" $
        F.c_marquise_connect consumer
    result <- f connection
    F.c_marquise_close connection
    return result


runMarquise :: B.ByteString -> Rational -> Marquise a -> IO a
runMarquise broker poll (Marquise a) =
    withConsumer broker poll $ \consumer ->
        withConnection consumer $ \connection ->
            runReaderT a connection

-- | Helper continuation passing compuation that builds a connection, cstring
-- arrays and associated size, then checks the continuation's result for -1
withConnFieldsValuesLength :: String
                           -> [(B.ByteString, B.ByteString)]
                           -> (Ptr F.MarquiseConnection
                            -> Ptr CString
                            -> Ptr CString
                            -> CSize
                            -> IO CInt)
                           -> Marquise ()
withConnFieldsValuesLength c_function tag_pairs f = do
    connection <- ask
    liftIO $  void $
        withCStringArray (map fst tag_pairs) $ \fields_ptr ->
        withCStringArray (map snd tag_pairs) $ \values_ptr ->
            E.throwIfMinus1 c_function $ f connection
                                           fields_ptr
                                           values_ptr
                                           (fromIntegral $ length tag_pairs)
  where
    withCStringArray :: [B.ByteString] -> (Ptr CString -> IO a) -> IO a
    withCStringArray bss f' = do
            (runCont . mapM cont) (map B.useAsCString bss) $ \cstrings ->
                SV.unsafeWith (SV.fromList cstrings) f'

sendText :: [(B.ByteString, B.ByteString)] -> Word64 -> B.ByteString -> Marquise ()
sendText tag_pairs timestamp text =
    withConnFieldsValuesLength "marquise_send_text" tag_pairs $ \c f v l ->
            B.useAsCStringLen text $ \(text_ptr, text_len) ->
                    F.c_marquise_send_text c f v l
                                           text_ptr
                                           (fromIntegral text_len)
                                           timestamp

sendInt :: [(B.ByteString, B.ByteString)] -> Word64 -> Int64 -> Marquise ()
sendInt tag_pairs timestamp int =
    withConnFieldsValuesLength "marquise_send_text" tag_pairs $ \c f v l ->
                F.c_marquise_send_int c f v l int timestamp

sendReal :: [(B.ByteString, B.ByteString)] -> Word64 -> Rational -> Marquise ()
sendReal tag_pairs timestamp r =
    withConnFieldsValuesLength "marquise_send_real" tag_pairs $ \c f v l ->
                F.c_marquise_send_real c f v l (fromRational r) timestamp

sendCounter :: [(B.ByteString, B.ByteString)] -> Word64 -> Marquise ()
sendCounter tag_pairs timestamp =
    withConnFieldsValuesLength "marquise_send_counter" tag_pairs $ \c f v l ->
                F.c_marquise_send_counter c f v l timestamp

sendBinary :: [(B.ByteString, B.ByteString)] -> Word64 -> B.ByteString -> Marquise ()
sendBinary tag_pairs timestamp text =
    withConnFieldsValuesLength "marquise_send_binary" tag_pairs $ \c f v l ->
            B.useAsCStringLen text $ \(text_ptr, text_len) ->
                    F.c_marquise_send_binary c f v l
                                           text_ptr
                                           (fromIntegral text_len)
                                           timestamp

timeNow :: Marquise Word64
timeNow =
    liftIO $ (+) <$> ((1000000000*) . sec) <*> nsec <$> getTime Realtime
    >>= return . fromIntegral
