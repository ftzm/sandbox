{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module NFTrie where

import qualified Data.Trie as T
import Control.DeepSeq
import Control.DeepSeq.Generics
import Generics.Deriving.TH
import Data.ByteString (ByteString)

$(deriveAll0     ''T.Trie)

-- NFData Trie Orphan Instance
instance NFData (T.Trie ByteString) where
  rnf = genericRnfV1
