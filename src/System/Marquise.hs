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
    sendText,
    sendInt,
    sendReal,
    sendCounter,
    sendBinary,
) where

import System.Marquise.Error (checkError, checkError_, checkError')
import qualified System.Marquise.Error as E
import qualified System.Marquise.FFI as F

import Control.Applicative
import Control.Monad
import Data.ByteString as B
import Foreign hiding (Pool, newPool, void)
import Foreign.C.String
import Foreign.C.Types
import Control.Exception (bracket, onException)
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Either
import Data.Word
import Data.Time

withConsumer :: B.ByteString -> Rational -> (Ptr F.ASConsumer -> IO a) -> IO a
withConsumer broker poll f =
    B.useAsCString broker $ \broker_cstr -> do
        consumer <- F.c_marquise_consumer_new broker_cstr (fromRational poll)
        result <- f consumer
        F.c_marquise_consumer_shutdown consumer
        return result

withConnection :: Ptr F.ASConsumer -> (Ptr F.ASConnection -> IO a) -> IO a
withConnection consumer f = do
    connection <- F.c_marquise_connect consumer
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

sendText :: [(String, String)] -> Word64 -> Marquise ()
sendText = undefined
sendInt = undefined
sendReal = undefined
sendCounter = undefined
sendBinary = undefined
