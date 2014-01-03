{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Marquise.FFI where

import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

-- | Represents a marquise_connection (void *)
data MarquiseConnection
-- | Represents a marquise_consumer (void *)
data MarquiseConsumer

foreign import ccall unsafe "marquise.h marquise_consumer_new"
    c_marquise_consumer_new :: CString -> CDouble -> IO (Ptr MarquiseConsumer)

foreign import ccall unsafe "marquise.h marquise_connect"
    c_marquise_connect :: Ptr MarquiseConsumer -> IO (Ptr MarquiseConnection)

foreign import ccall unsafe "marquise.h marquise_close"
    c_marquise_close :: Ptr MarquiseConnection -> IO ()

foreign import ccall unsafe "marquise.h marquise_consumer_shutdown"
    c_marquise_consumer_shutdown :: Ptr MarquiseConsumer -> IO ()

foreign import ccall unsafe "marquise.h marquise_send_text"
    c_marquise_send_text :: Ptr MarquiseConnection
                         -> Ptr CString -- ^ Source fields
                         -> Ptr CString -- ^ Source values
                         -> CSize       -- ^ Source count
                         -> CString     -- ^ Text to send
                         -> CSize       -- ^ Text lenght
                         -> Word64      -- ^ Timestamp
                         -> IO (CInt)

foreign import ccall unsafe "marquise.h marquise_send_int"
    c_marquise_send_int :: Ptr MarquiseConnection
                        -> Ptr CString -- ^ Source fields
                        -> Ptr CString -- ^ Source values
                        -> CSize       -- ^ Source count
                        -> Int64       -- ^ Int to send
                        -> Word64      -- ^ Timestamp
                        -> IO (CInt)

foreign import ccall unsafe "marquise.h marquise_send_real"
    c_marquise_send_real :: Ptr MarquiseConnection
                         -> Ptr CString -- ^ Source fields
                         -> Ptr CString -- ^ Source values
                         -> CSize       -- ^ Source count
                         -> CDouble     -- ^ Real to send
                         -> Word64      -- ^ Timestamp
                         -> IO (CInt)


foreign import ccall unsafe "marquise.h marquise_send_counter"
    c_marquise_send_counter :: Ptr MarquiseConnection
                            -> Ptr CString -- ^ Source fields
                            -> Ptr CString -- ^ Source values
                            -> CSize       -- ^ Source count
                            -> Word64      -- ^ Timestamp
                            -> IO (CInt)

foreign import ccall unsafe "marquise.h marquise_send_binary"
    c_marquise_send_binary :: Ptr MarquiseConnection
                           -> Ptr CString -- ^ Source fields
                           -> Ptr CString -- ^ Source values
                           -> CSize       -- ^ Source count
                           -> CString     -- ^ Binary to send
                           -> CSize       -- ^ Binary length
                           -> Word64      -- ^ Timestamp
                           -> IO (CInt)

foreign import ccall unsafe "string.h strerror"
    c_strerror :: Errno -> IO (Ptr CChar)
