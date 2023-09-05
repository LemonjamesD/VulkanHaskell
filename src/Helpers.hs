{-# LANGUAGE CApiFFI #-}

module Helpers where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ConstPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Utils

foreign import capi "glfw_wrapped.h nullConstStr" raw_nullConstStr :: IO (ConstPtr CChar) 
foreign import capi "glfw_wrapped.h allocConstStr" raw_allocConstStr :: CString -> IO (ConstPtr CChar) 