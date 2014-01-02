{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Marquise.FFI where

import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

data ASConnection
data ASConsumer


foreign import ccall unsafe "marquise.h marquise_consumer_new"
    c_marquise_consumer_new :: CString -> CDouble -> IO (Ptr ASConsumer)

foreign import ccall unsafe "marquise.h marquise_connect"
    c_marquise_connect :: Ptr ASConsumer -> IO (Ptr ASConnection)

foreign import ccall unsafe "marquise.h marquise_close"
    c_marquise_close :: Ptr ASConnection -> IO ()

foreign import ccall unsafe "marquise.h marquise_consumer_shutdown"
    c_marquise_consumer_shutdown :: Ptr ASConsumer -> IO ()

foreign import ccall unsafe "marquise.h marquise_send_text"
    c_marquise_send_text :: Ptr ASConnection -> Ptr CString -> Ptr CString -> CSize -> CString -> CSize -> Word64 -> IO (CInt)

foreign import ccall unsafe "string.h strerror"
    c_strerror :: Errno -> IO (Ptr CChar)
