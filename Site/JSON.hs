{-
- I'm so, so sorry about this file. One day I'll revisit it and make it
- not a horrible abomination.
-}

module Site.JSON where

import Text.Parsec as P
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Char
import Control.Applicative
import Numeric
import Site.Animations
import Animations.LED

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            deriving (Eq, Ord, Show)

newtype JAry a = JAry { fromJAry :: [a]
                      } deriving (Eq, Ord, Show)

newtype JObj a = JObj { fromJObj :: [(String, a)]
                      } deriving (Eq, Ord, Show)

collapseEither :: [(Either a b)] -> Either a [b]
collapseEither xs = case foldr foldfunc (Nothing,[]) xs of
                        (Nothing,lst) -> Right lst
                        ((Just x),_) -> Left x
    where foldfunc (Left x) (_,lst) = (Just x, lst)
          foldfunc (Right x) (lefts,lst) = (lefts, x:lst)

parseBlend :: String -> Maybe BlendingMode
parseBlend "Add"      = Just add
parseBlend "Subtract" = Just sub
parseBlend "Multiply" = Just mult
parseBlend _          = Nothing


parseAnimMeta :: String -> Either String [(AnimMetadata,BlendingMode)]
parseAnimMeta input =
        case parseJson input of
            Right (JArray jary) -> collapseEither $ parseMetas $ fromJAry jary
            Left e -> Left $ show e
            _ -> Left "Valid json, but not an array"
    where parseMetas [] = []
          parseMetas ((JObject (JObj [("name",JString name),("params",(JArray (JAry jary))),("blendingmode",(JString mode))])):xs) =
                                                case name of
                                                    "Cylon Eye" -> case (jary,parseBlend mode) of
                                                                       ([ JObject (JObj [ ("double",(JNumber speed)) ])
                                                                        , JObject (JObj [ ("double",(JNumber size)) ])
                                                                        , JObject (JObj [ ("LED",JObject (JObj [ ("red",   (JNumber r))
                                                                                                               , ("green", (JNumber g))
                                                                                                               , ("blue",  (JNumber b))
                                                                                                               ]))
                                                                                        ])
                                                                        ],Just m) -> (Right $ (CylonEye speed size (LED (floor r) (floor g) (floor b)),m)):(parseMetas xs)
                                                                       _ -> [Left "Invalid animation parameters"]
                                                    "Set All"  -> case (jary,parseBlend mode) of
                                                                       ([ JObject (JObj [ ("LED",JObject (JObj [ ("red",   (JNumber r))
                                                                                                               , ("green", (JNumber g))
                                                                                                               , ("blue",  (JNumber b))
                                                                                                               ]))
                                                                                        ])
                                                                        ],Just m) -> (Right $ (SetAll (LED (floor r) (floor g) (floor b)),m)):(parseMetas xs)
                                                                       _ -> [Left "Invalid animation parameters"]
                                                    "Wave"     -> case (jary,parseBlend mode) of
                                                                       ([ JObject (JObj [ ("double",(JNumber speed)) ])
                                                                        , JObject (JObj [ ("double",(JNumber size)) ])
                                                                        , JObject (JObj [ ("double",(JNumber freq)) ])
                                                                        , JObject (JObj [ ("LED",JObject (JObj [ ("red",   (JNumber r))
                                                                                                               , ("green", (JNumber g))
                                                                                                               , ("blue",  (JNumber b))
                                                                                                               ]))
                                                                                        ])
                                                                        ],Just m) -> (Right $ (Wave speed size freq (LED (floor r) (floor g) (floor b)),m)):(parseMetas xs)
                                                                       _ -> [Left "Invalid animation parameters"]
                                                    "Spectrum" -> case (jary,parseBlend mode) of
                                                                       ([ JObject (JObj [ ("LED",JObject (JObj [ ("red",   (JNumber r))
                                                                                                               , ("green", (JNumber g))
                                                                                                               , ("blue",  (JNumber b))
                                                                                                               ]))
                                                                                        ])
                                                                        ],Just m) -> (Right $ (Spectrum (LED (floor r) (floor g) (floor b)),m)):(parseMetas xs)
                                                    "Volume"   -> case (jary,parseBlend mode) of
                                                                       ([ JObject (JObj [ ("LED",JObject (JObj [ ("red",   (JNumber r))
                                                                                                               , ("green", (JNumber g))
                                                                                                               , ("blue",  (JNumber b))
                                                                                                               ]))
                                                                                        ])
                                                                        ],Just m) -> (Right $ (Volume (LED (floor r) (floor g) (floor b)),m)):(parseMetas xs)
                                                                       _ -> [Left "Invalid animation parameters"]
                                                    _ -> [Left $ "Unknown animation: " ++ name]
          parseMetas _ = [Left "Invalid json found"]


parseJson :: String -> Either ParseError JValue
parseJson input = parse p_text "" input

p_text :: CharParser () JValue
p_text = spaces *> text
     P.<?> "JSON text"
    where text = JObject <$> p_object
            P.<|> JArray <$> p_array

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JString <$> p_string
          P.<|> JNumber <$> p_number
          P.<|> JObject <$> p_object
          P.<|> JArray  <$> p_array
          P.<|> JBool   <$> p_bool
          P.<|> JNull   <$  string "null"
          P.<?> "JSON value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
   P.<|> False <$ string "false"

p_value_choice = value <* spaces
  where value = choice [ JString <$> p_string
                       , JNumber <$> p_number
                       , JObject <$> p_object
                       , JArray <$> p_array
                       , JBool <$> p_bool
                       , JNull <$ string "null"
                       ]
                <?> "JSON value"

p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

-- file: ch16/JSONParsec.hs
p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (P.many jchar)
    where jchar = char '\\' *> (p_escape P.<|> p_unicode)
              P.<|> satisfy (`notElem` "\"\\")

-- file: ch16/JSONParsec.hs
p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

-- file: ch16/JSONParsec.hs
p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x
