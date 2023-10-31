-- SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Data.NormalForm.DNF
  ( Literal(..)
  , DNF(..)
  , dnfFalse, dnfTrue
  , dnfSingleton
  , dnfNeg, dnfPos
  , dnfAnd, dnfOr
  ) where

import Data.NormalForm.Internal

import Control.DeepSeq (NFData)

import Data.Aeson
import Data.Aeson.TH

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)

import qualified Data.Set as Set
import Data.Set (Set)

import GHC.Generics (Generic)


-- | A single literal of a term may be positive or negative
data Literal a =
    LiteralNeg { literal :: a }
  | LiteralPos { literal :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)
  deriving anyclass (Hashable, Binary, NFData)

deriveJSON litAesonOptions ''Literal


-- | A disjunctive normal form is a disjunction (set) of terms, a term being a conjunction (set) of literals
newtype DNF a = DNF { dnfTerms :: Set (Set (Literal a)) }
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)
  deriving anyclass (Binary, Hashable, NFData)

-- hack to ensure DNF is in the type environment at the reify used in mkParseJSON
$(return [])

instance ToJSON a => ToJSON (DNF a) where
  toJSON = $(mkToJSON dnfAesonOptions ''DNF)
instance (FromJSON a, Ord a) => FromJSON (DNF a) where
  parseJSON = $(mkParseJSON dnfAesonOptions ''DNF)


-- | The disjunction of no terms corresponds to False
dnfFalse :: DNF a
dnfFalse = DNF Set.empty

-- | The disjunction of a single term consisting of no literal corresponds to True
dnfTrue :: DNF a
dnfTrue = DNF $ Set.singleton Set.empty

-- | The disjunctive normal form of a single literal
dnfSingleton :: Literal a -> DNF a
dnfSingleton = DNF . Set.singleton . Set.singleton

-- | A single negative literal
dnfNeg :: a -> DNF a
dnfNeg = dnfSingleton . LiteralNeg

-- | A single positive literal
dnfPos :: a -> DNF a
dnfPos = dnfSingleton . LiteralPos


infixr 3 `dnfAnd`
-- | The intersection of two disjunctive normal forms is the cartesian product of the terms of both formulae
dnfAnd :: Ord a => DNF a -> DNF a -> DNF a
dnfAnd  (DNF a) (DNF b) = DNF . Set.fromList $ do
  aConj <- Set.toList a
  bConj <- Set.toList b
  return $ aConj `Set.union` bConj

infixr 2 `dnfOr`
-- | The union of two disjunctive normal forms is just the union of the formulae sets of terms
dnfOr :: Ord a => DNF a -> DNF a -> DNF a
dnfOr (DNF a) (DNF b) = DNF $ a <> b
