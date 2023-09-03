module Main where

import GlfwBindings
import VulkanBindings

import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef
import Control.Monad.IO.Unlift

main :: IO ()
main = do
  ctx <- initWindowCtx
  runWindow doWindowActions ctx

doWindowActions :: Window ()
doWindowActions = do
  glfwInit
  liftIO $ putStrLn "Hello, World!"
  glfwWindowHint glfwClientApi glfwNoApi
  glfwCreateWindow 100 100 "Hello, World"
  
  liftIO $ UnliftIO $ vulkanEnumerateInstanceExtensionProperties Nothing 0 Nothing
  
  eventLoop

eventLoop :: Window ()
eventLoop = do
  glfwPollEvents
  
  -- Read error codes
  (resultCode, description) <- glfwGetError
  liftIO $ putStrLn $ show resultCode
  liftIO $ putStrLn $ show description
  liftIO $ threadDelay 1000000

  eventLoop