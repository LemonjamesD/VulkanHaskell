module Main where

import Glfw.GlfwWrapped
import WindowMonad
import VulkanBindings

import Control.Monad.IO.Class
import Control.Concurrent

main :: IO ()
main = do
  ctx <- initWindowCtx
  runWindow doWindowActions ctx

doWindowActions :: Window ()
doWindowActions = do
  glfwInit
  glfwWindowHint glfwClientApi glfwNoApi
  glfwWindowHint glfwResizable glfwFalse
  glfwCreateWindow 100 100 "Hello, World"
  
  _ <- withVulkan
    (\_ -> do
            result <- vulkanEnumerateInstanceExtensionProperties Nothing 0 Nothing
            liftIO $ putStrLn $ "Extensions: " ++ show result)
  
  eventLoop

eventLoop :: Window ()
eventLoop = do
  shouldClose <- glfwWindowShouldClose
  if shouldClose then do
    glfwDestroyWindow
    glfwTerminate
  else do
    glfwPollEvents
    glfwSwapBuffers
  
    -- Read error codes
    (resultCode, description) <- glfwGetError
    liftIO $ putStrLn $ "Error Code: " ++ show resultCode
    liftIO $ putStrLn $ "Description: " ++ show description
    liftIO $ threadDelay 1000000

    eventLoop