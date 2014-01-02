{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Marquise
(
    -- *Exceptions
    -- |
    -- This library should only ever throw a 'MarquiseError'.
    E.MarquiseError(MarquiseError),
    E.cFunction,
    E.errno,
    E.strerror,
    runMarquise,
    sendText,
    sendInt,
    sendReal,
    sendCounter,
    sendBinary,
) where

import qualified System.Marquise.Error as E
import qualified System.Marquise.FFI as F

import Control.Monad.Cont
import Foreign hiding (Pool, newPool, void)
import Foreign.C.String
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as SV

withConsumer :: B.ByteString -> Rational -> (Ptr F.ASConsumer -> IO a) -> IO a
withConsumer broker poll f =
    B.useAsCString broker $ \broker_cstr -> do
        consumer <- E.throwIfNull "marquise_consumer_new" $
            F.c_marquise_consumer_new broker_cstr (fromRational poll)
        result <- f consumer
        F.c_marquise_consumer_shutdown consumer
        return result

withConnection :: Ptr F.ASConsumer -> (Ptr F.ASConnection -> IO a) -> IO a
withConnection consumer f = do
    connection <- E.throwIfNull "marquise_connect" $
        F.c_marquise_connect consumer
    result <- f connection
    F.c_marquise_close connection
    return result

newtype Marquise a = Marquise (ReaderT (Ptr F.ASConnection) IO a)
    deriving (Monad, MonadIO, MonadReader (Ptr F.ASConnection))

runMarquise :: B.ByteString -> Rational -> Marquise a -> IO a
runMarquise broker poll (Marquise a) =
    withConsumer broker poll $ \consumer ->
        withConnection consumer $ \connection ->
            runReaderT a connection

sendText :: [(B.ByteString, B.ByteString)] -> Word64 -> B.ByteString -> Marquise ()
sendText tag_pairs timestamp text = do
    connection <- ask
    liftIO $ void $
        withCStringArray (map fst tag_pairs) $ \fields_ptr ->
        withCStringArray (map snd tag_pairs) $ \values_ptr ->
            B.useAsCStringLen text $ \(text_ptr, text_len) ->
                E.throwIfMinus1 "marquise_send_text" $
                    F.c_marquise_send_text connection
                                           fields_ptr
                                           values_ptr
                                           (fromIntegral $ length tag_pairs)
                                           text_ptr
                                           (fromIntegral text_len)
                                           timestamp
  where
    withCStringArray :: [B.ByteString] -> (Ptr CString -> IO a) -> IO a
    withCStringArray bss f = do
        let continuations = map B.useAsCString bss in
            runCont (mapM cont continuations) $ \cstrings -> 
                SV.unsafeWith (SV.fromList cstrings) $ \ptr -> f ptr

sendInt = undefined
sendReal = undefined
sendCounter = undefined
sendBinary = undefined
