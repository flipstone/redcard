module Happstack.Aeson.Message where

import            Data.Aeson
import            Data.Aeson.Encode.Pretty
import            Happstack.Server

instance ToMessage Value where
  toContentType _ = "application/json"
  toMessage = encodePretty

