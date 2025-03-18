{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.Script.Decoder
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Script.Decoder
  ( -- * Decoder
    Decoder (..),
    decodeQuery,

    -- ** Parser
    skipSpaces,
    skipSpaces1,
    simpleDecoderParser,
    asClauseParser,
    andChainParser,
    fieldAccessParser,
    returningFieldsParser,
    returningClauseParser,
    decoderParser,

    -- * Internal
    FieldAccess (..),
    Primitive (..),
    ScriptComponent (..),
    DynamicScriptComponent (..),
    getFieldDyn,
  )
where

import Data.Kind
import Data.Maybe
import Data.Typeable
import GHC.TypeLits
import Text.Parsec
import Text.Parsec.String

data FieldAccess = FieldAccess String String
  deriving (Show, Eq)

data Primitive = IntPrimitive Int
  deriving (Show, Eq)

class (KnownSymbol (ComponentID a)) => ScriptComponent a where
  type ComponentID a :: Symbol

  type Schema a :: [Type]

  getField :: String -> a -> Maybe Primitive

  queryId :: String
  queryId = symbolVal (Proxy @(ComponentID a))

data DynamicScriptComponent = forall a. (ScriptComponent a) => DynamicScriptComponent a

getFieldDyn :: String -> DynamicScriptComponent -> Maybe Primitive
getFieldDyn s (DynamicScriptComponent a) = getField s a

data Decoder a where
  FetchDecoder :: String -> Decoder (String, DynamicScriptComponent)
  AsDecoder :: Decoder (String, DynamicScriptComponent) -> String -> Decoder ()
  AndDecoder :: Decoder () -> Decoder () -> Decoder ()
  ReturningDecoder :: Decoder a -> [FieldAccess] -> Decoder [Primitive]

skipSpaces :: Parser ()
skipSpaces = skipMany (oneOf " \t")

skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 (oneOf " \t")

simpleDecoderParser :: Parser (Either (Decoder (String, DynamicScriptComponent)) (Decoder ()))
simpleDecoderParser = do
  skipSpaces
  _ <- string "FETCH"
  skipSpaces1
  qId <- many1 (noneOf " \t\n\r")
  let base = FetchDecoder qId
  alias <- optionMaybe (try asClauseParser)
  case alias of
    Just a -> return $ Right (AsDecoder base a)
    Nothing -> return $ Left base

asClauseParser :: Parser String
asClauseParser = try $ do
  skipSpaces
  _ <- string "AS"
  skipSpaces1
  alias <- many1 (noneOf " \t\n\r")
  return alias

andChainParser :: Parser (Decoder ())
andChainParser = do
  first <- simpleDecoderParser
  let first' = case first of
        Right d -> d
        Left _ -> error "TODO"
  rest <- many $ try $ do
    skipSpaces
    _ <- string "AND"
    skipSpaces
    next <- simpleDecoderParser
    case next of
      Right d -> return d
      Left _ -> error "TODO"
  return $ foldl AndDecoder first' rest

fieldAccessParser :: Parser FieldAccess
fieldAccessParser = do
  skipSpaces
  alias <- many1 (noneOf ". ,()")
  _ <- char '.'
  field <- many1 (noneOf " ,()")
  return $ FieldAccess alias field

returningFieldsParser :: Parser [FieldAccess]
returningFieldsParser = fieldAccessParser `sepBy1` (skipSpaces >> char ',' >> skipSpaces)

returningClauseParser :: Parser [FieldAccess]
returningClauseParser = do
  skipSpaces
  _ <- string "RETURNING"
  skipSpaces
  _ <- char '('
  fields <- returningFieldsParser
  _ <- char ')'
  return fields

decoderParser :: Parser (Decoder [Primitive])
decoderParser = do
  chain <- andChainParser
  res <- optionMaybe returningClauseParser
  skipSpaces
  eof
  return . ReturningDecoder chain $ fromMaybe [] res

decodeQuery :: String -> Decoder [Primitive]
decodeQuery input =
  case parse decoderParser "" input of
    Left err -> error ("Failed to parse query: " ++ show err)
    Right dec -> dec
