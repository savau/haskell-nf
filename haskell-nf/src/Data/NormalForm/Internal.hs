-- SPDX-FileCopyrightText: 2023 Sarah Vaupel <sarah.vaupel@uniworx.de>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Data.NormalForm.Internal where

import Data.Aeson

import Text.Casing (fromHumps, toKebab)


dnfAesonOptions :: Options
dnfAesonOptions = defaultOptions
  { constructorTagModifier = camelToKebab
  , fieldLabelModifier     = camelToKebab
  } where camelToKebab     = toKebab . fromHumps
