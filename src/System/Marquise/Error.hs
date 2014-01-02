{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module System.Marquise.Error
(
    MarquiseError(MarquiseError),
    errno,
    cFunction,
    strerror,
    throwIfNull,
    throwIfMinus1,
) where

import Control.Exception
import Data.Typeable
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import System.Marquise.FFI as F

-- | An error indicated by librados, usually in the form of a negative return
-- value
data MarquiseError = MarquiseError
    { errno     :: Int    -- ^ Error number (positive)
    , cFunction :: String -- ^ The underlying c function that called
    , strerror  :: String -- ^ The "nice" error message from strerror
    } deriving (Eq, Ord, Typeable)

instance Show MarquiseError where
    show MarquiseError{..} = "rados-haskell: rados error in '" ++
        cFunction ++ "', errno " ++ show errno ++ ": '" ++ strerror ++ "'"

instance Exception MarquiseError

-- Handle a ceph Errno, which is an errno that must be negated before being
-- passed to strerror. Otherwise, treat the result a positive int and pass it
-- straight through.
--
-- This is needed for a few methods like rados_read that throw an error or
-- return the bytes read via the same CInt.
throwIf :: (a -> Bool) -> String -> IO a -> IO a
throwIf condition c_function action = do
    r <- action
    if (condition r) then throwMarquiseError c_function
                     else return r

throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull = throwIf (== nullPtr)

throwIfMinus1 :: String -> IO CInt -> IO CInt
throwIfMinus1 = throwIf (== -1)

throwMarquiseError :: String -> IO a
throwMarquiseError c_function = do
    (Errno n) <- getErrno
    strerror <- peekCString =<< F.c_strerror (Errno n)
    throwIO $ MarquiseError (fromIntegral n) c_function strerror
