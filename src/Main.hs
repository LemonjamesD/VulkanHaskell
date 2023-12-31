module Main where

import Glfw.GlfwWrapped
import Vulkan.VulkanWrapped
import WindowMonad
import VulkanMonad

import Control.Monad.IO.Class
import Control.Concurrent

main :: IO ()
main = do
  ctx <- initWindowCtx
  runWindow doWindowActions ctx

doWindowActions :: Window ()
doWindowActions = do
  initWindow
  withVulkan
    (\_ -> do
            result <- vulkanEnumerateInstanceExtensionProperties Nothing 0 Nothing
            liftIO $ putStrLn $ "Extensions: " ++ show result)
  
  eventLoop

initWindow :: Window ()
initWindow = do
  glfwInit
  glfwWindowHint glfwClientApi glfwNoApi
  glfwWindowHint glfwResizable glfwFalse
  glfwCreateWindow 800 600 "Hello, World"

cleanUp :: Window ()
cleanUp = do
  glfwDestroyWindow
  glfwTerminate

eventLoop :: Window ()
eventLoop = do
  shouldClose <- glfwWindowShouldClose
  if shouldClose then
    cleanUp
  else do
    runWindowCode
    eventLoop

runWindowCode :: Window ()
runWindowCode = do
    glfwPollEvents
    glfwSwapBuffers
  
    -- Read error codes
    (resultCode, description) <- glfwGetError
    liftIO $ putStrLn $ "Error Code: " ++ show resultCode
    liftIO $ putStrLn $ "Description: " ++ show description
    liftIO $ threadDelay 1000000