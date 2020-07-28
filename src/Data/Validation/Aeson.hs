module Data.Validation.Aeson where

import            Control.Monad.Identity

import            Data.Aeson
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Lazy as LazyBS
import qualified  Data.HashMap.Strict as HashMap
import qualified  Data.Map.Strict as Map
import qualified  Data.Set as Set
import qualified  Data.Text as Text
import qualified  Data.Vector as Vec

import            Data.Validation.Types

decodeValidJSON :: Validator a -> LazyBS.ByteString -> ValidationResult a
decodeValidJSON validator input =
  runIdentity (decodeValidJSONT (liftV validator) input)

decodeValidJSONStrict :: Validator a -> BS.ByteString -> ValidationResult a
decodeValidJSONStrict validator input =
  runIdentity (decodeValidJSONStrictT (liftV validator) input)

decodeValidJSONT :: Applicative m
                 => ValidatorT m a -> LazyBS.ByteString -> m (ValidationResult a)
decodeValidJSONT validator input =
  case eitherDecode input of
  Left err -> pure $ Invalid (errMessage $ Text.pack err)
  Right value -> runValidatorT validator (value :: Value)

decodeValidJSONStrictT :: Applicative m
                       => ValidatorT m a -> BS.ByteString -> m (ValidationResult a)
decodeValidJSONStrictT validator input =
  case eitherDecodeStrict input of
  Left err -> pure $ Invalid (errMessage $ Text.pack err)
  Right value -> runValidatorT validator (value :: Value)


instance Validatable Value where
  inputText (String text) = Just text
  inputText _ = Nothing

  inputNull Null = IsNull
  inputNull _ = NotNull

  inputBool (Bool True) = Just True
  inputBool (Bool False) = Just False
  inputBool _ = Nothing

  arrayItems (Array items) = Just items
  arrayItems _ = Nothing

  scientificNumber (Number sci) = Just sci
  scientificNumber _ = Nothing

  lookupChild attrName (Object hmap) = LookupResult $
                                       HashMap.lookup attrName hmap
  lookupChild _ _ = InvalidLookup

instance ToJSON Errors where
  toJSON (Messages set) = Array
                        . Vec.fromList
                        . map toJSON
                        . Set.toList
                        $ set

  toJSON (Group attrs) = Object
                       . HashMap.fromList
                       . Map.toList
                       . Map.map toJSON
                       $ attrs

