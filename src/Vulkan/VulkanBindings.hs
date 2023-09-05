{-# LANGUAGE CApiFFI #-}

module Vulkan.VulkanBindings where

import Vulkan.VulkanTypes

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.ConstPtr

foreign import capi "vulkan/vulkan.h vkEnumerateInstanceExtensionProperties" raw_enumerateInstanceExtensionProperties :: ConstPtr CChar -> Ptr CUInt -> Ptr RawVkExtensionProperties -> IO RawVkResult