{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module System.Marquise.Error
(
    MarquiseError(MarquiseError),
    errno,
    cFunction,
    strerror,
    checkError,
    checkError',
    checkError_,
) where

import Control.Exception
import Control.Monad (void)
import Data.Typeable
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
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
checkError :: String -> IO CInt -> IO Int
checkError function action = do
    checkError' function action >>= either throwIO return

checkError' :: String -> IO CInt -> IO (Either MarquiseError Int)
checkError' function action = do
    n <- action
    if n < 0
        then do
            let errno = (-n)
            strerror <- peekCString =<< F.c_strerror (Errno errno)
            return $ Left $ MarquiseError (fromIntegral errno) function strerror
        else return $ Right $ fromIntegral n

checkError_ :: String -> IO CInt -> IO ()
checkError_ desc action = void $ checkError desc action
