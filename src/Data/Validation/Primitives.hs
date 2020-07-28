{-# LANGUAGE Rank2Types #-}
module Data.Validation.Primitives where

import            Control.Monad (join)
import            Data.Char
import            Data.Convertible
import            Data.Foldable
import            Data.Scientific
import qualified  Data.Text as Text
import qualified  Data.Vector as Vec

import            Data.Validation.Types

liftResult ::  ValidationResult a -> Validator a
liftResult result = Validator (const result)

validRead :: String -> ReadS a -> Validator a
validRead err reader = do
  text <- string

  case reader (Text.unpack text) of
    [(value, "")] -> pure value
    _ -> fail err

bool :: Validator Bool
bool =
  Validator $ \input ->
    case inputBool input of
      Just True -> Valid True
      Just False -> Valid False
      _ -> Invalid (errMessage "must_be_bool")

string :: Validator Text.Text
string =
 Validator $ \input ->
   case inputText input of
     Just t -> Valid (Text.strip t)
     _ -> Invalid (errMessage "must_be_string")

notBlank :: Validator Text.Text -> Validator Text.Text
notBlank validator =
  Validator $ \value ->
    case run validator value of
    Valid t | Text.all isSpace t -> Invalid (errMessage "must_not_be_blank")
    result -> result

atMost :: Int -> Validator Text.Text -> Validator Text.Text
atMost len validator =
  Validator $ \value ->
    case run validator value of
    Valid t | (Text.length t > len) -> Invalid (errMessage $ Text.pack ("max_length_" ++ (show len)))
    result -> result

atLeast :: Int -> Validator Text.Text -> Validator Text.Text
atLeast len validator =
  Validator $ \value ->
    case run validator value of
    Valid t | (Text.length t < len) -> Invalid (errMessage $ Text.pack ("min_length_" ++ (show len)))
    result -> result

exactly :: Int -> Validator Text.Text -> Validator Text.Text
exactly len validator =
  Validator $ \value ->
    case run validator value of
    Valid t | Text.length t /= len -> Invalid (errMessage $ Text.pack ("must_be_length_" ++ (show len)))
    result -> result

numeric :: Validator Scientific
numeric = Validator $ \input ->
            case scientificNumber input of
            Just n -> Valid n
            _ -> Invalid (errMessage "must_be_numeric")

integer :: (Integral i, Bounded i) => Validator i
integer = do
  number <- numeric
  case toBoundedInteger number of
    Just int -> return int
    _ -> fail "must_be_integer"

double :: Validator Double
double = fromRational . toRational <$> numeric

nonEmpty :: Validator [a] -> Validator [a]
nonEmpty validator =
  Validator $ \input ->
    case (run validator input) of
    Valid [] -> Invalid (errMessage $ Text.pack ("must_have_at_least_one_in_list"))
    result -> result

arrayOf :: Validator a -> Validator [a]
arrayOf validator =
  Validator $ \input ->
    case arrayItems input of
      Just items ->
        let itemValidator = runIndexed (pure <$> validator)
        in fold (Vec.imap itemValidator items)
      _ -> Invalid (errMessage "must_be_array")

ifInvalid :: Validator a
          -> Validator b
          -> Validator (Either a b)
ifInvalid validA validB = Validator $ \input ->
  case run validA input of
  Valid a -> Valid (Left a)
  Invalid _ -> Right <$> run validB input

validConversion :: Convertible a b => Validator a -> Validator b
validConversion validator = do
  a <- validator

  case safeConvert a of
    Right b -> pure b
    Left err -> fail (convErrorMessage err)

runIndexed :: Validatable input
           => Validator a -> Int -> input -> ValidationResult a
runIndexed validator idx value = run (nestIndex idx validator) value

nestIndex :: Int -> Validator a -> Validator a
nestIndex = nest . Text.pack . show

nest :: Text.Text -> Validator a -> Validator a
nest attr validator = mapErrors (nestErrors attr) `mapResult` validator

mustBeNull :: Validator ()
mustBeNull = Validator $ \input ->
  case inputNull input of
    IsNull -> Valid ()
    NotNull -> Invalid (errMessage "must_be_null")
    InvalidNull text -> Invalid (errMessage text)

canBeNull :: Validator ()
canBeNull = Validator $ \input ->
  case inputNull input of
    IsNull -> Valid ()
    NotNull -> Valid ()
    InvalidNull text -> Invalid (errMessage text)

nullable' :: Validator a -> Validator (Maybe a)
nullable' validator =
      either (const Nothing) Just
  <$> (mustBeNull `ifInvalid` validator)

nullable :: Validator a -> Validator (Maybe a)
nullable validator = Validator $ \input ->
  case run canBeNull input of
    Valid _ -> run (nullable' validator) input
    Invalid err -> Invalid err

required :: Text.Text -> Validator a -> Validator a
attrName `required` validator = validateAttr attrName req
  where req (Just subvalue) = run validator subvalue
        req Nothing = Invalid (errMessage "must_be_present")

optional :: Text.Text -> Validator a -> Validator (Maybe a)
attrName `optional` validator = validateAttr attrName opt
  where opt (Just value) = Just <$> run validator value
        opt Nothing = Valid Nothing

optionalAndNullable :: Text.Text -> Validator a -> Validator (Maybe a)
attrName `optionalAndNullable` validator = join <$> attrName `optional` (nullable validator)

infixr 5 `required`
infixr 5 `optional`

notPresent :: Text.Text -> Validator ()
notPresent attr = validateAttr attr $ isNotPresent
  where isNotPresent (Just _) = Invalid (errMessage "must_not_be_present")
        isNotPresent Nothing = Valid ()

validateAttr :: Text.Text
             -> (forall input. Validatable input => Maybe input -> ValidationResult a)
             -> Validator a
validateAttr attrName f =
  Validator $ \input ->
    case lookupChild attrName input of
      LookupResult result ->
        mapErrors (nestErrors attrName) (f result)

      InvalidLookup ->
        Invalid (errMessage "must_be_object")
