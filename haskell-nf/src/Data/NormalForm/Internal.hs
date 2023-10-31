-- SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Data.NormalForm.Internal where

import Data.Aeson

import Text.Casing (fromHumps, toKebab)


camelToKebab :: String -> String
camelToKebab = toKebab . fromHumps

litAesonOptions, dnfAesonOptions :: Options
litAesonOptions = defaultOptions
  { constructorTagModifier = camelToKebab
  , fieldLabelModifier     = camelToKebab
  , sumEncoding            = TaggedObject "signum" "literal"
  }
dnfAesonOptions = defaultOptions
  { constructorTagModifier = camelToKebab
  , fieldLabelModifier     = camelToKebab
  , tagSingleConstructors  = True  -- for compatibility with future CNF
  , sumEncoding            = TaggedObject "type" "formula"
  }
