module Glfw.GlfwWrapped where

import Glfw.GlfwBindings
import Helpers
import WindowMonad

import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ConstPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

glfwTrue :: Int
glfwTrue = 1
glfwFalse :: Int
glfwFalse = 0

glfwClientApi :: Int
glfwClientApi = fromCInt raw_glfwClientApi
glfwNoApi :: Int
glfwNoApi = fromCInt raw_glfwNoApi

glfwResizable :: Int
glfwResizable = fromCInt raw_glfwResizable

glfwInit :: Window ()
glfwInit = do
  insertWindowAction InitWindow
  result <- liftIO raw_glfwInit
  case result of
    1 -> return ()
    _ -> liftIO (throw (FailedToGLFWInit (fromCInt result)))

glfwWindowHint :: Int -> Int -> Window ()
glfwWindowHint hint value = do
  insertWindowAction (SetWindowHint hint value)
  _ <- liftIO $ raw_glfwWindowHint (toCInt hint) (toCInt value)
  return ()

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

glfwPollEvents :: Window ()
glfwPollEvents = do
  insertWindowAction (PolledEvent)
  liftIO $ raw_glfwPollEvents

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

glfwWindowShouldClose :: Window Bool
glfwWindowShouldClose = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  result <- liftIO $ raw_glfwWindowShouldClose unwrapped
  return (case result of
    1 -> True
    _ -> False)

glfwDestroyWindow :: Window ()
glfwDestroyWindow = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  liftIO $ raw_glfwDestroyWindow unwrapped

glfwSwapBuffers :: Window ()
glfwSwapBuffers = do
  ctx <- getWindowCtx
  windowValue <- liftIO $ readIORef $ realWindow ctx
  unwrapped <- liftIO $ maybe (throw WindowNotInitialized) pure windowValue
  _ <- liftIO $ raw_glfwSwapBuffers unwrapped
  return ()

glfwTerminate :: Window ()
glfwTerminate = liftIO $ raw_glfwTerminate

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
