{-# LANGUAGE CPP #-}
module Data.Validation.Aeson where

import            Control.Monad.Identity

import            Data.Aeson
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Lazy as LazyBS
#if MIN_VERSION_aeson(2,0,0)
import qualified  Data.Aeson.Key as Key
import qualified  Data.Aeson.KeyMap as KeyMap
#else
import qualified  Data.HashMap.Strict as HashMap
#endif
import qualified  Data.Map.Strict as Map
import qualified  Data.Set as Set
import qualified  Data.Text as Text
import qualified  Data.Vector as Vec

import            Data.Validation.Types

decodeValidJSON :: Validator Value a -> LazyBS.ByteString -> ValidationResult a
decodeValidJSON validator input =
  runIdentity (decodeValidJSONT (liftV validator) input)

decodeValidJSONStrict :: Validator Value a -> BS.ByteString -> ValidationResult a
decodeValidJSONStrict validator input =
  runIdentity (decodeValidJSONStrictT (liftV validator) input)

decodeValidJSONT :: Applicative m
                 => ValidatorT Value m a
                 -> LazyBS.ByteString
                 -> m (ValidationResult a)
decodeValidJSONT validator input =
  case eitherDecode input of
  Left err -> pure $ Invalid (errMessage $ Text.pack err)
  Right value -> runValidatorT validator (value :: Value)

decodeValidJSONStrictT :: Applicative m
                       => ValidatorT Value m a
                       -> BS.ByteString
                       -> m (ValidationResult a)
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
#if MIN_VERSION_aeson(2,0,0)
                                       KeyMap.lookup (Key.fromText attrName) hmap
#else
                                       HashMap.lookup attrName hmap
#endif
  lookupChild _ _ = InvalidLookup

instance ToJSON Errors where
  toJSON (Messages set) = Array
                        . Vec.fromList
                        . map toJSON
                        . Set.toList
                        $ set

  toJSON (Group attrs) = Object
#if MIN_VERSION_aeson(2,0,0)
                       . KeyMap.fromList
#else
                       . HashMap.fromList
#endif
                       . Map.toList
#if MIN_VERSION_aeson(2,0,0)
                       . Map.mapKeys Key.fromText
#endif
                       . Map.map toJSON
                       $ attrs

