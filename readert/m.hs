-- These together allow the nested forall in the function argument to ioBox.
-- The forall at the front is explicit in normal haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}

{-# LANGUAGE MultiParamTypeClasses #-} -- needed to, you guessed it, have more
-- than one param with a typeclass


-- {-# LANGUAGE AllowAmbiguousTypes #-} -- needed for unused type variable in typeclass

{-# LANGUAGE FlexibleInstances #-} -- allows referencing arbitrary nested types
-- in typeclass instances

{-# LANGUAGE TypeFamilies #-} -- weirdly needed for ~
-- The ~, by the way, asserts type equality. so we can do (m ~ IO)

{-# LANGUAGE FlexibleContexts #-} -- use this to allow non-type variable
-- arguments in constraints

{-# LANGUAGE DeriveGeneric #-} -- guess what this is for

{-# LANGUAGE UndecidableInstances #-}
-- needed to allow the r's in the DB typeclass instance for ReaderT, which
-- occur in the contstraint but not in the instance head. The check is,
-- unfortunately, too dumb.
-- Apparently this is a typical problem when using typeclasses as essentially
-- constraint synonyms

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Reader
import Control.Monad.IO.Class

import GHC.Generics
import Data.Generics.Product.Typed

-------------------------------------------------------------------------------
-- RankNTypes

class (Monad m) => MonadOi m where
  oi :: String -> m ()

instance MonadOi IO where
  oi s = putStrLn $ "oi, " ++ s ++ "!"

oimate :: MonadOi m => String -> m ()
oimate s = oi $ "mate, " ++ s

-- this one has has the MonadOi typeclass constraint, but also is explicitly IO
oimateIO :: (MonadOi m, m ~ IO) => String -> m ()
oimateIO s = oi $ "IOmate, " ++ s

ioBox :: forall m. (m ~ IO) => (forall m. MonadOi m => String -> m ()) -> m ()
ioBox f = f "input"

useIoBox :: IO ()
useIoBox = ioBox oimate -- this compiles
--useIoBox = ioBox oimateIO -- but this doesn't
-- This is because the type signature for the argument to ioBox requres the
-- function to be fully polymorphic, so that even if the monad in the function
-- ultimately resolves to IO, it may not be specified as such in the function itself.

-- >>> useIoBox
-- oi, IOmate, input!

main :: IO ()
main = oimate "bruv"

-- >>> main
-- oi, mate, bruv!

-------------------------------------------------------------------------------
-- ReaderT and Has

data User = User String deriving Show

data Env m = Env
  { getUserFunc :: (String -> m User)
  } deriving (Generic)

getUserIO :: String -> IO User
getUserIO s = return $ User s

ioEnv :: Env IO
ioEnv = Env getUserIO

class Monad m => DB m where
  getUser :: String -> m User

-------------------------------------------------------------------------------
-- hand-written has boilerplate

class (Monad m) => HasGetUser m r where
  supplyGetUser :: r -> (String -> m User)

--HasType makes this unnecessary
instance (Monad m) => HasGetUser m (Env m)  where
  supplyGetUser = getUserFunc

--replaced with HasType version
-- instance (HasGetUser m r, Monad m) => DB (ReaderT r m) where
--   getUser = getUser <$> ask

-------------------------------------------------------------------------------
-- DB instance with HasType

-- instance (Monad m, HasType (String -> m User) r, MonadReader r m) => DB m where
--   getUser = getUser <$> ask

instance (Monad m, HasType (String -> m User) r) => DB (ReaderT r m) where
  getUser = getUser <$> ask

exclaimUser :: DB m => m User
exclaimUser = do
  User name <- getUser "dave"
  return $ User $ name ++ "!"

doStuff :: IO ()
doStuff = do
  user <- runReaderT exclaimUser ioEnv
  putStrLn $ show user
