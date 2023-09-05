{-# LANGUAGE CApiFFI #-}

module Helpers where

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.ConstPtr
import Data.Int

foreign import capi "glfw_wrapped.h nullConstStr" raw_nullConstStr :: IO (ConstPtr CChar) 
foreign import capi "glfw_wrapped.h allocConstStr" raw_allocConstStr :: CString -> IO (ConstPtr CChar) 

toCInt :: Int -> CInt
toCInt int = CInt (fromIntegral int :: Int32)

fromCInt :: CInt -> Int
fromCInt (CInt cint) = fromIntegral cint :: Int
