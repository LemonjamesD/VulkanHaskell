module Vulkan.VulkanWrapped where

import VulkanMonad
import Helpers
import Vulkan.VulkanTypes
import Vulkan.VulkanBindings

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Utils
import Control.Monad.IO.Class

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