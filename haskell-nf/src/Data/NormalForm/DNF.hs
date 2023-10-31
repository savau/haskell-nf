-- SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Data.NormalForm.DNF
  ( Literal(..)
  ) where

import Control.DeepSeq (NFData)

import Data.Binary (Binary)
import Data.Data (Data)
import Data.Hashable (Hashable)

import GHC.Generics (Generic)


-- | A single literal of a term may or may not be negated
data Literal a =
    Literal        { literal :: a }
  | NegatedLiteral { literal :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic)
  deriving anyclass (Hashable, Binary, NFData)
