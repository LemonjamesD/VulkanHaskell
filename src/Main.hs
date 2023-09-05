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
  glfwWindowHint glfwClientApi glfwNoApi
  glfwCreateWindow 100 100 "Hello, World"
  
  result <- liftIO $ vulkanToIO $ vulkanEnumerateInstanceExtensionProperties Nothing 0 Nothing
  liftIO $ putStrLn $ "Extensions: " ++ show result
  
  eventLoop

eventLoop :: Window ()
eventLoop = do
  shouldClose <- glfwWindowShouldClose
  if shouldClose then
    glfwDestroyWindow
  else do
    glfwPollEvents
  
    -- Read error codes
    (resultCode, description) <- glfwGetError
    liftIO $ putStrLn $ "Error Code: " ++ show resultCode
    liftIO $ putStrLn $ "Description: " ++ show description
    liftIO $ threadDelay 1000000

    eventLoop