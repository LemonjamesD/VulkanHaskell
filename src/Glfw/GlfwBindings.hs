{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Glfw.GlfwBindings where

import Glfw.GlfwTypes

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ConstPtr

-- GLFW Constants

foreign import capi "GLFW/glfw3.h value GLFW_VERSION_MAJOR" raw_glfwVersionMajor :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_VERSION_MINOR" raw_glfwVersionMinor :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_VERSION_REVISION" raw_glfwVersionRevision :: CInt

foreign import capi "GLFW/glfw3.h value GLFW_TRUE" raw_glfwTrue :: GlfwBool
foreign import capi "GLFW/glfw3.h value GLFW_FALSE" raw_glfwFalse :: GlfwBool

foreign import capi "GLFW/glfw3.h value GLFW_CLIENT_API" raw_glfwClientApi :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_NO_API" raw_glfwNoApi :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_RESIZABLE" raw_glfwResizable :: CInt

-- GLFW Functions

foreign import capi "GLFW/glfw3.h glfwInit" raw_glfwInit :: IO CInt
foreign import capi "GLFW/glfw3.h glfwWindowHint" raw_glfwWindowHint :: CInt -> CInt -> IO ()
foreign import capi "GLFW/glfw3.h glfwCreateWindow" raw_glfwCreateWindow :: CInt -> CInt -> CString -> Ptr RawGlfwMonitor -> Ptr RawGlfwWindow -> IO (Ptr RawGlfwWindow)
foreign import capi "GLFW/glfw3.h glfwPollEvents" raw_glfwPollEvents :: IO ()
foreign import capi "GLFW/glfw3.h glfwGetError" raw_glfwGetError :: Ptr (ConstPtr CChar) -> IO CInt
foreign import capi "GLFW/glfw3.h glfwWindowShouldClose" raw_glfwWindowShouldClose :: Ptr RawGlfwWindow -> IO CInt
foreign import capi "GLFW/glfw3.h glfwDestroyWindow" raw_glfwDestroyWindow :: Ptr RawGlfwWindow -> IO ()
foreign import capi "GLFW/glfw3.h glfwSwapBuffers" raw_glfwSwapBuffers :: Ptr RawGlfwWindow -> IO ()
foreign import capi "GLFW/glfw3.h glfwTerminate" raw_glfwTerminate :: IO ()
foreign import capi safe "GLFW/glfw3.h glfwSetErrorCallback" raw_glfwSetErrorCallback :: FunPtr (CInt -> ConstPtr CChar -> IO ()) -> IO (FunPtr (CInt -> ConstPtr CChar -> ()))
