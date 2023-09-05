{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module VulkanBindings where

import Helpers
import WindowMonad

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.ConstPtr
import Foreign.CStorable
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
} deriving (Generic)
instance CStorable RawVkExtensionProperties
instance Storable RawVkExtensionProperties where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data VulkanExtensionProperties = VulkanExtensionProperties {
  extensionName :: String,
  specVersion :: Int
}

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

-- Vulkan Functions

foreign import capi "vulkan/vulkan.h vkEnumerateInstanceExtensionProperties" raw_enumerateInstanceExtensionProperties :: ConstPtr CChar -> Ptr CUInt -> Ptr RawVkExtensionProperties -> IO RawVkResult

vulkanEnumerateInstanceExtensionProperties :: Maybe String -> Int -> Maybe VulkanExtensionProperties -> Vulkan Int
vulkanEnumerateInstanceExtensionProperties layer count extensionProperties = do
  -- if Nothing then nullptr if something then make it a CString
  c_layer <- liftIO $ maybe (raw_nullConstStr) 
    (\x -> do
              cstringed <- newCString x
              raw_allocConstStr cstringed) layer
  c_extensionProperties <- liftIO $ 
    maybe (pure $ nullPtr @RawVkExtensionProperties) (\props -> do
                                        c_string <- newCString $ extensionName props
                                        let c_int = CUInt $ fromIntegral $ specVersion props
                                        new $ RawVkExtensionProperties c_string c_int) extensionProperties
  let c_count = CUInt (fromIntegral count)
  alloced_count <- liftIO $ new c_count
  result <- liftIO $ raw_enumerateInstanceExtensionProperties c_layer alloced_count c_extensionProperties
  
  return (fromIntegral result)