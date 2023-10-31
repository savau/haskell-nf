-- SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Data.NormalForm.DNF
  ( Literal(..)
  , DNF(..)
  , dnfFalse
  , dnfSingleton
  , dnfLit
  , dnfNeg
  , dnfAnd
  , dnfOr
  ) where

import Control.DeepSeq (NFData)

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import Data.Set (Set)

import GHC.Generics (Generic)


-- | A single literal of a term may or may not be negated
data Literal a =
    Literal        { literal :: a }
  | NegatedLiteral { literal :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)
  deriving anyclass (Hashable, Binary, NFData)


newtype DNF a = DNF { dnfTerms :: Set (Set (Literal a)) }
  deriving (Eq, Ord, Show, Read, Data, Generic, Typeable)
  deriving anyclass (Binary, Hashable, NFData)

dnfFalse :: DNF a
dnfFalse = DNF Set.empty

dnfSingleton :: Literal a -> DNF a
dnfSingleton = DNF . Set.singleton . Set.singleton

dnfLit, dnfNeg :: a -> DNF a
dnfLit = dnfSingleton . Literal
dnfNeg = dnfSingleton . NegatedLiteral

infixr 3 `dnfAnd`
infixr 2 `dnfOr`

dnfAnd, dnfOr :: Ord a => DNF a -> DNF a -> DNF a
dnfAnd  (DNF a) (DNF b) = DNF . Set.fromList $ do
  aConj <- Set.toList a
  bConj <- Set.toList b
  return $ aConj `Set.union` bConj
dnfOr (DNF a) (DNF b) = DNF $ a <> b
