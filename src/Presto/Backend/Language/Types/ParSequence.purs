{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 This file is part of JUSPAY Platform.
 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commerical licenses, contact <info@juspay.in>.
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 be liable for all damages without limitation, which is caused by the
 ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 The end user has NO right to claim any indemnification based on its use
 of Licensed Software. See the GNU Affero General Public License for more details.
 You should have received a copy of the GNU Affero General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Presto.Backend.Language.Types.ParSequence where

import Prelude

import Control.Monad.Eff.Exception (Error, error, message)
import Data.Either (Either(..), either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Maybe (Maybe)
import Presto.Backend.Language.Types.EitherEx (class CustomEitherEx, EitherEx(RightEx, LeftEx), eitherEx)
import Presto.Backend.Language.Types.MaybeEx (MaybeEx, fromMaybeEx, toMaybeEx)
import Presto.Core.Utils.Encoding (defaultEncode, defaultDecode)

-- TODO: you can use this data type to add more typed errors.
data ParError
  = ParError String

toParError :: Error -> ParError
toParError = ParError <<< message

fromParError :: ParError -> Error
fromParError (ParError strError) = error strError

toParseqMaybeResult :: forall a. Either Error (Maybe a) -> EitherEx ParError (MaybeEx a)
toParseqMaybeResult = either (LeftEx <<< toParError) (RightEx <<< toMaybeEx)

fromParseqMaybeResult :: forall a. EitherEx ParError (MaybeEx a) -> Either Error (Maybe a)
fromParseqMaybeResult = eitherEx (Left <<< fromParError) (Right <<< fromMaybeEx)


derive instance genericParError :: Generic ParError _
instance decodeParError         :: Decode  ParError where decode  = defaultDecode
instance encodeParError         :: Encode  ParError where encode  = defaultEncode
instance eqParError             :: Eq      ParError where eq      = GEq.genericEq
instance showParError           :: Show    ParError where show    = GShow.genericShow
instance ordParError            :: Ord     ParError where compare = GOrd.genericCompare

instance customExErrorParError :: CustomEitherEx Error ParError a where
  fromCustomEitherEx (LeftEx (ParError errorMsg)) = Left $ error errorMsg
  fromCustomEitherEx (RightEx a)                 = Right a
  toCustomEitherEx = either (LeftEx <<< ParError <<< message) RightEx

