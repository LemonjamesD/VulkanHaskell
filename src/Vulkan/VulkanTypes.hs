{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Vulkan.VulkanTypes where

import GHC.Generics
import Foreign.CStorable
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

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

