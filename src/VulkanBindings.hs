{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module VulkanBindings where

import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ConstPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Utils
import Control.Monad.IO.Unlift

data VulkanCtx = VulkanCtx {
  vulkanActions :: IORef [VulkanActions]
}
data VulkanActions = EnumerateInstanceExtensionProperties

-- data VulkanErrors =
--   deriving (Show, Typeable)

-- instance Exception VulkanErrors

newtype Vulkan a = 
  Vulkan { runVulkan :: VulkanCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT VulkanCtx IO

-- Vulkan Datas

type RawVkResult = CInt

data RawVkExtensionProperties = RawVkExtensionProperties {
  c_extensionName :: CString,
  c_specVersion :: CUInt
}
instance Storable RawVkExtensionProperties

data VulkanExtensionProperties = VulkanExtensionProperties {
  extensionName :: String,
  specVersion :: Int
}

getVulkanCtx :: Vulkan VulkanCtx
getVulkanCtx = Vulkan (\ctx -> pure ctx)

vulkanToIO :: Vulkan a -> IO ()
vulkanToIO vulkan = do
  io <- \ctx -> pure ctx
  return ()
  

-- Vulkan Functions

foreign import capi "vulkan/vulkan.h vkEnumerateInstanceExtensionProperties" raw_enumerateInstanceExtensionProperties :: ConstPtr CChar -> Ptr CUInt -> Ptr RawVkExtensionProperties -> RawVkResult

vulkanEnumerateInstanceExtensionProperties :: Maybe String -> Int -> Maybe VulkanExtensionProperties -> Vulkan ()
vulkanEnumerateInstanceExtensionProperties layer count extensionProperties = do
  -- if Nothing then nullptr if something then make it a CString
  c_layer <- liftIO $ maybe (pure $ nullPtr) newCString layer
  c_extensionProperties <- liftIO $ 
    maybe (pure $ nullPtr @RawVkExtensionProperties) (\props -> do
                                        c_string <- newCString $ extensionName props
                                        let c_int = CUInt $ fromIntegral $ specVersion props
                                        new $ RawVkExtensionProperties c_string c_int) extensionProperties
  let c_count = CUInt (fromIntegral count)
  
  return ()