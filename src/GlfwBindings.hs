{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module GlfwBindings where

import VulkanBindings
import Helpers

import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Data.Int
import Foreign.C.ConstPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

data WindowCtx = WindowCtx {
  windowActions :: IORef [WindowActions],
  realWindow :: IORef (Maybe (Ptr RawGlfwWindow))
}
data WindowActions = InitWindow | SetWindowHint Int Int | CreateWindow Int Int String | PolledEvent

data WindowErrors =
  FailedToGLFWInit Int
  | CreateWindowIsNullPtr
  | WindowNotInitialized
  deriving (Show, Typeable)

instance Exception WindowErrors

newtype Window a = 
  Window { runWindow :: WindowCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT WindowCtx IO

initWindowCtx :: IO WindowCtx
initWindowCtx = do
  actions <- newIORef []
  window <- newIORef Nothing
  pure $ WindowCtx { 
                  windowActions = actions,
                  realWindow = window
                }

getWindowCtx :: Window WindowCtx
getWindowCtx = Window (\ctx -> pure ctx)

insertWindowAction :: WindowActions -> Window ()
insertWindowAction action = do
  ctx <- getWindowCtx
  liftIO $ modifyIORef' (windowActions ctx) (action:)

insertRealWindow :: Ptr RawGlfwWindow -> Window ()
insertRealWindow window = do
  ctx <- getWindowCtx
  liftIO $ modifyIORef' (realWindow ctx) (\_ -> Just window)

-- GLFW Constants

foreign import capi "GLFW/glfw3.h value GLFW_VERSION_MAJOR" raw_glfwVersionMajor :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_VERSION_MINOR" raw_glfwVersionMinor :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_VERSION_REVISION" raw_glfwVersionRevision :: CInt

foreign import capi "GLFW/glfw3.h value GLFW_TRUE" raw_glfwTrue :: GlfwBool
foreign import capi "GLFW/glfw3.h value GLFW_FALSE" raw_glfwFalse :: GlfwBool

foreign import capi "GLFW/glfw3.h value GLFW_CLIENT_API" raw_glfwClientApi :: CInt
foreign import capi "GLFW/glfw3.h value GLFW_NO_API" raw_glfwNoApi :: CInt
glfwClientApi :: Int
glfwClientApi = fromCInt raw_glfwClientApi
glfwNoApi :: Int
glfwNoApi = fromCInt raw_glfwNoApi

-- It's an alias of Int
type GlfwBool = CInt
type Void = CSize
type CVoid = Void
type RawGlfwMonitor = ()
type RawGlfwWindow = ()

-- Helper function because lazy
toCInt :: Int -> CInt
toCInt int = CInt (fromIntegral int :: Int32)

fromCInt :: CInt -> Int
fromCInt (CInt cint) = fromIntegral cint :: Int

-- Actual GLFW Functions

foreign import capi "GLFW/glfw3.h glfwInit" raw_glfwInit :: IO CInt

glfwInit :: Window ()
glfwInit = do
  insertWindowAction InitWindow
  result <- liftIO raw_glfwInit
  case result of
    1 -> return ()
    _ -> liftIO (throw (FailedToGLFWInit (fromCInt result)))


foreign import capi "GLFW/glfw3.h glfwWindowHint" raw_glfwWindowHint :: CInt -> CInt -> IO ()

glfwWindowHint :: Int -> Int -> Window ()
glfwWindowHint hint value = do
  insertWindowAction (SetWindowHint hint value)
  _ <- liftIO $ raw_glfwWindowHint (toCInt hint) (toCInt value)
  return ()

foreign import capi "GLFW/glfw3.h glfwCreateWindow" raw_glfwCreateWindow :: CInt -> CInt -> CString -> Ptr RawGlfwMonitor -> Ptr RawGlfwWindow -> IO (Ptr RawGlfwWindow)

-- This needs to be updated to actually match the raw functions signature
glfwCreateWindow :: Int -> Int -> String -> Window ()
glfwCreateWindow w h name = do
  insertWindowAction (CreateWindow w h name)
  c_name <- liftIO $ newCString name
  window <- liftIO $ raw_glfwCreateWindow (toCInt w) (toCInt h) c_name nullPtr nullPtr
  _ <- liftIO $ if window == nullPtr then
    throw (CreateWindowIsNullPtr)
  else
    pure ()
  insertRealWindow window
  
foreign import capi "GLFW/glfw3.h glfwPollEvents" raw_glfwPollEvents :: IO ()

glfwPollEvents :: Window ()
glfwPollEvents = do
  insertWindowAction (PolledEvent)
  liftIO $ raw_glfwPollEvents

foreign import capi "GLFW/glfw3.h glfwGetError" raw_glfwGetError :: Ptr (ConstPtr CChar) -> IO CInt

glfwGetError :: Window (Int, Maybe String)
glfwGetError = do
  (errorCode, errorString) <- liftIO $
    alloca @(ConstPtr CChar) $ \bufferPtrPtr -> do
      errorCode <- raw_glfwGetError bufferPtrPtr
      bufferPtr <- peek bufferPtrPtr
      let unConstBufferPtr = unConstPtr bufferPtr
      stringed <- liftIO 
        $ if unConstBufferPtr == nullPtr then
           return Nothing
          else do
           stringed <- peekCString unConstBufferPtr
           return (Just stringed)
      let inted = fromCInt errorCode
      return (inted, stringed)
  return (errorCode, errorString)
  
foreign import capi "GLFW/glfw3.h glfwWindowShouldClose" raw_glfwWindowShouldClose :: Ptr RawGlfwWindow -> IO CInt

glfwWindowShouldClose :: Window Bool
glfwWindowShouldClose = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  result <- liftIO $ raw_glfwWindowShouldClose unwrapped
  return (case result of
    1 -> True
    _ -> False)
  
foreign import capi "GLFW/glfw3.h glfwDestroyWindow" raw_glfwDestroyWindow :: Ptr RawGlfwWindow -> IO ()

glfwDestroyWindow :: Window ()
glfwDestroyWindow = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  liftIO $ raw_glfwDestroyWindow unwrapped

foreign import capi "GLFW/glfw3.h glfwSwapBuffers" raw_glfwSwapBuffers :: Ptr RawGlfwWindow -> IO ()

glfwSwapBuffers :: Window ()
glfwSwapBuffers = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  result <- liftIO $ raw_glfwSwapBuffers unwrapped
  return ()

-- foreign import capi safe "GLFW/glfw3.h glfwSetErrorCallback" raw_glfwSetErrorCallback :: FunPtr (CInt -> ConstPtr CChar -> IO ()) -> IO (FunPtr (CInt -> ConstPtr CChar -> ()))

-- glfwSetErrorCallback :: (Int -> String -> ()) -> Window ()
-- glfwSetErrorCallback f = do
--   func <- pure $ castPtrToFunPtr $ liftIO $ new
--     (\error description 
--       -> do
--             unConsted <- unConstPtr description
--             stringed <- peekCString unConsted
--             inted <- fromCInt error
--             f inted stringed)
--   _ <- liftIO $ raw_glfwSetErrorCallback func
--   pure ()
