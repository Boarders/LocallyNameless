{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

module LN.Parser where

import Data.Text hiding (empty, foldr, foldl')
import Text.Megaparsec hiding (sepEndBy1)
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Prelude hiding (unlines)
import Control.Monad (void)
import Data.Char
import Control.Applicative (liftA2)
import Data.List (foldl')

import LN.Expression


type Parser = Parsec Void Text


-- |
-- This ignores spaces and tabs.
spaceP :: Parser ()
spaceP = void $ takeWhileP (Just "space chars") (liftA2 (||) (== ' ') (== '\t'))

-- |
-- This ignores all whitespace.
wSpaceP :: Parser ()
wSpaceP = space1

-- |
wSpaceSomeP :: Parser ()
wSpaceSomeP = C.space

lineComment :: Parser ()
lineComment = empty

blockComment :: Parser ()
blockComment = empty

--wsIgnore :: Parser ()
--wsIgnore = L.space wSpaceP lineComment blockComment

--sIgnore :: Parser ()
--sIgnore = L.space spaceP lineComment blockComment

lexemeS :: Parser a -> Parser a
lexemeS = lexeme spaceP

lexemeSomeWS :: Parser a -> Parser a
lexemeSomeWS = lexeme wSpaceSomeP

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme wSpaceP

ident :: Parser Text
ident = lexemeS (cons <$> lowerChar <*> takeWhileP Nothing (isAlphaNum))

equals :: Parser ()
equals = void $ lexemeS (char ('=')) 

leftCurly :: Parser ()
leftCurly = void $ lexemeSomeWS (char ('{')) 

rightCurly :: Parser ()
rightCurly = void $ lexemeSomeWS (char ('}'))

leftBracket :: Parser ()
leftBracket = void $ lexemeS (char ('('))

rightBracket :: Parser ()
rightBracket = void $ lexemeSomeWS (char (')'))

bracketed :: Parser a -> Parser a
bracketed p =
  do
    _   <- try leftBracket
    res <- p
    _   <- rightBracket
    pure res

keyword :: Text -> Parser ()
keyword t = void $ lexemeS (chunk t)


var :: Parser (Term Text)
var = Var <$> ident


lam :: Parser (Term Text)
lam =
  do
    void $ lexemeS (char '\\')
    vars <- some ident
    void $ lexemeS (char '.')
    body <- parseExpr
    let lamExpr = foldr Lam body vars
    pure lamExpr


parseUnit :: Parser (Term Text)
parseUnit = bracketed parseExpr <|> lam <|> var

app :: Parser (Term Text)
app =
  do
    left  <- parseUnit
    rest  <- some parseUnit
    let appExpr = foldl' App left rest
    pure appExpr


parseExpr :: Parser (Term Text)
parseExpr =
  choice
    [ lam
    , (try app)
    , var
    , bracketed parseExpr
    ]



sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do{ x <- p
                    ; do{ _ <- sep
                        ; xs <- sepEndBy p sep
                        ; return (x:xs)
                        }
                      <|> return [x]
                    }

{-

programExample :: Text
programExample = unlines ["f = {1}", "", "g = {let {x = 1; y = 2} 2}"]

blockExample :: Text
blockExample = "g = let {x = a; y = b} w"

lamEx :: Text
lamEx = "\\x . x"
-}
