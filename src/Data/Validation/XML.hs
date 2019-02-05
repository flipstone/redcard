module Data.Validation.XML
  ( decodeValidXML )
  where

import            Control.Applicative

import qualified  Data.ByteString.Lazy as LazyBS
import qualified  Data.Map as Map
import            Data.Maybe
import            Data.Scientific
import qualified  Data.Text as Text
import qualified  Data.Vector as Vec
import            Text.XML

import            Data.Validation.Types

decodeValidXML :: Validator a -> LazyBS.ByteString -> ValidationResult a
decodeValidXML validator input =
  case parseLBS def input of
  Left err -> Invalid (errMessage $ Text.pack (show err))
  Right doc -> run validator (VDoc doc)

data VXML =
    VDoc Document
  | VElem Element
  | VText Text.Text

instance Validatable VXML where
  inputBool        = getBool
  inputText        = getText
  inputNull        = getNull
  arrayItems       = getArray
  scientificNumber = getNumber
  lookupChild      = getChild

getBool :: VXML -> Maybe Bool
getBool vxml = getText vxml >>= parseBool

getText :: VXML -> Maybe Text.Text
getText (VDoc (Document _ root _)) = getText (VElem root)
getText (VElem (Element _ _ nodes)) = Just $ childrenText nodes
getText (VText text) = Just $ text

getNull :: VXML -> Maybe ()
getNull (VElem (Element _ _ [])) = Just ()
getNull _ = Nothing

getArray :: VXML -> Maybe (Vec.Vector VXML)
getArray (VElem (Element _ _ nodes)) =
  Just $ Vec.fromList (mapMaybe (fmap VElem . nodeElem) nodes)

getArray (VDoc (Document _ root _)) = getArray (VElem root)
getArray (VText _) = Nothing

getNumber :: VXML -> Maybe Scientific
getNumber vxml = getText vxml >>= parseNumber

getChild :: Text.Text -> VXML -> Lookup VXML
getChild _ (VText _) = InvalidLookup

getChild childName (VElem (Element _ attrs nodes)) =
       (VElem <$> findNode childName nodes)
   <|> (VText <$> findAttr childName attrs)

getChild childName (VDoc (Document prologue root epilogue)) =
  case Text.splitAt 1 childName of
    ("?", instructionName) -> LookupResult $
          findProcessingInstruction instructionName (prologueBefore prologue)
      <|> findProcessingInstruction instructionName (prologueAfter prologue)
      <|> findProcessingInstruction instructionName epilogue

    _ -> getChild childName (VElem root)

findProcessingInstruction :: Text.Text -> [Miscellaneous] -> Maybe VXML
findProcessingInstruction name misc =
    VText <$> listToMaybe (mapMaybe getData misc)
  where
    getData (MiscInstruction (Instruction n d)) | name == n = Just d
    getData _ = Nothing

findAttr :: Text.Text -> Map.Map Name Text.Text -> Lookup Text.Text
findAttr attrName attrs =
    LookupResult (Map.lookup attrFullName attrs)
  where
    attrFullName = Name attrName Nothing Nothing


findNode :: Text.Text -> [Node] -> Lookup Element
findNode elemName nodes = case matches of
                          [el] -> LookupResult (Just el)
                          _ -> LookupResult Nothing

  where matches = mapMaybe (elemWithName elemName) nodes

nodeElem :: Node -> Maybe Element
nodeElem (NodeElement el) = Just el
nodeElem _ = Nothing

elemWithName :: Text.Text -> Node -> Maybe Element
elemWithName elemName node = do
  el <- nodeElem node

  if elemName == nameLocalName (elementName el) then
    Just el
  else
    Nothing

parseNumber :: Text.Text -> Maybe Scientific
parseNumber text =
  case reads (Text.unpack (Text.strip text)) of
    (n, "") : _ -> Just n
    _           -> Nothing

parseBool :: Text.Text -> Maybe Bool
parseBool text =
  case reads (Text.unpack (Text.strip text)) of
    (n, "") : _ -> Just n
    _           -> Nothing

data Content = Content Text.Text
             | NotContent

instance Semigroup Content where
  (Content t) <> (Content t') = Content (t <> t')
  _ <> _                      = NotContent

instance Monoid Content where
  mempty = Content ""

contentText :: Content -> Text.Text
contentText (Content text) = text
contentText _ = ""

nodeContent :: Node -> Content
nodeContent (NodeContent text) = Content text
nodeContent _ = NotContent

childrenText :: [Node] -> Text.Text
childrenText nodes = contentText (foldMap nodeContent nodes)

