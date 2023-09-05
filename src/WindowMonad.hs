{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module WindowMonad where

import Glfw.GlfwTypes

import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Foreign.Ptr

data WindowCtx = WindowCtx {
  windowActions :: IORef [WindowActions],
  realWindow :: IORef (Maybe (Ptr RawGlfwWindow))
}
data WindowActions = InitWindow | SetWindowHint Int Int | CreateWindow Int Int String | PolledEvent

data WindowErrors =
  FailedToGLFWInit Int
  | CreateWindowIsNullPtr
  | WindowNotInitialized
  deriving (Show, Typeable)

instance Exception WindowErrors

newtype Window a = 
  Window { runWindow :: WindowCtx -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT WindowCtx IO

initWindowCtx :: IO WindowCtx
initWindowCtx = do
  actions <- newIORef []
  window <- newIORef Nothing
  pure $ WindowCtx { 
                  windowActions = actions,
                  realWindow = window
                }

getWindowCtx :: Window WindowCtx
getWindowCtx = Window (\ctx -> pure ctx)

insertWindowAction :: WindowActions -> Window ()
insertWindowAction action = do
  ctx <- getWindowCtx
  liftIO $ modifyIORef' (windowActions ctx) (action:)

insertRealWindow :: Ptr RawGlfwWindow -> Window ()
insertRealWindow window = do
  ctx <- getWindowCtx
  liftIO $ modifyIORef' (realWindow ctx) (\_ -> Just window)
