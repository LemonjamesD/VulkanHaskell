{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module VulkanMonad where

import WindowMonad

import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Monad.IO.Unlift

data VulkanCtx = VulkanCtx {
  vulkanActions :: IORef [VulkanActions]
}
data VulkanActions = EnumerateInstanceExtensionProperties

data VulkanErrors = None
  deriving (Show, Typeable)

instance Exception VulkanErrors

newtype Vulkan a = 
  Vulkan { runVulkan :: VulkanCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT VulkanCtx IO

initVulkanCtx :: IO VulkanCtx
initVulkanCtx = do
  actions <- newIORef []
  pure $ VulkanCtx { 
                  vulkanActions = actions
                }

getVulkanCtx :: Vulkan VulkanCtx
getVulkanCtx = Vulkan (\ctx -> pure ctx)

vulkanToIO :: Vulkan a -> IO a
vulkanToIO vulkan = do
  ctx <- initVulkanCtx
  runVulkan vulkan ctx

withVulkan :: (() -> Vulkan a) -> Window a
withVulkan f = liftIO $ vulkanToIO $ f ()
