{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Identity

-------------------------------------------------------------------------------
-- Monad Experimentation

func :: Int -> Reader Int Int
func input = do
  i <- ask
  return $ i + input

runFunc :: Int
runFunc = runIdentity $ runReaderT (func 4) 3

-- >>> runFunc
-- Identity 7

func' :: Int -> StateT Int (ReaderT String IO) String
func' input = do
  setting <- ask
  liftIO $ print $ "This is the setting: " <> setting
  initialState <- get
  liftIO $ print $ "Initial state: " <> show initialState
  put 3
  secondState <- get
  liftIO $ print $ "Second state: " <> show secondState
  return "hi"

func'' :: (MonadState Int m, MonadReader String m, MonadIO m) => Int -> m String
func'' input = do
  setting <- ask
  liftIO $ print $ "This is the setting: " <> setting
  initialState <- get
  liftIO $ print $ "Initial state: " <> show initialState
  put 3
  secondState <- get
  liftIO $ print $ "Second state: " <> show secondState
  return "hi"

fonc :: (MonadReader Int m) => m String
fonc = undefined
