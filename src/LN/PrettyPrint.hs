{-# LANGUAGE OverloadedStrings #-}

module LN.PrettyPrint
  ( -- * Configuration
    PrettyConfig(..)
  , defaultConfig
  , asciiConfig

  -- * Pretty printing
  , prettyTerm
  , prettyTermWith
  , prettyTermDoc
  , prettyTermDocWith

  -- * Rendering
  , renderPlain
  , renderColouredString
  , renderColouredText
  ) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Prettyprinter.Render.Terminal as Term
import qualified Prettyprinter.Render.Text as Plain
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import LN.Expression (Term(..))

data PrettyConfig = PrettyConfig
  { useUnicode :: Bool      -- ^ Use λ instead of \
  , colourBoundVars :: Bool  -- ^ Different colour for bound vs free variables
  , maxWidth :: Int         -- ^ Maximum line width
  } deriving (Eq, Show)

-- | Default configuration (Unicode enabled, colours enabled, 80 char width)
defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig
  { useUnicode = True
  , colourBoundVars = True
  , maxWidth = 80
  }

-- | ASCII configuration (backslash lambdas)
asciiConfig :: PrettyConfig
asciiConfig = defaultConfig { useUnicode = False }

-- | Precedence levels for parenthesization
data Prec = PrecLam | PrecApp | PrecAtom
  deriving (Eq, Ord)

-- | Pretty print with default configuration
prettyTerm :: (Ord a, Show a) => Term a -> String
prettyTerm = renderPlain . prettyTermDoc

-- | Pretty print with custom configuration
prettyTermWith :: (Ord a, Show a) => PrettyConfig -> Term a -> String
prettyTermWith config = renderPlain . prettyTermDocWith config

-- | Pretty print to a Doc with default configuration
prettyTermDoc :: (Ord a, Show a) => Term a -> Doc AnsiStyle
prettyTermDoc = prettyTermDocWith defaultConfig

-- | Pretty print to a Doc with custom configuration
prettyTermDocWith :: (Ord a, Show a) => PrettyConfig -> Term a -> Doc AnsiStyle
prettyTermDocWith config term = prettyPrec config Set.empty PrecLam term

-- | Remove surrounding quotes from a string
unquote :: String -> String
unquote ('"':rest) = case reverse rest of
  '"':rrest -> reverse rrest
  _ -> '"':rest
unquote s = s

-- | Core pretty printing function with precedence and bound variable tracking
prettyPrec :: (Ord a, Show a)
           => PrettyConfig
           -> Set a           -- ^ Bound variables in scope
           -> Prec            -- ^ Current precedence level
           -> Term a
           -> Doc AnsiStyle
prettyPrec config boundVars prec term = case term of
  Var v ->
    let colourFn = if colourBoundVars config && Set.member v boundVars
                  then colourBound
                  else colourFree
    in colourFn (pretty (unquote (show v)))

  Lam x body ->
    let (binders, body') = collectLambdas (Lam x body)
        boundVars' = foldr Set.insert boundVars binders
        lambdaSym = if useUnicode config then pretty ("λ" :: String) else pretty ("\\" :: String)
        binderDocs = map (pretty . unquote . show) binders
        doc = colourLambda lambdaSym
           <> hsep binderDocs
           <> colourLambda (pretty ("." :: String))
           <+> prettyPrec config boundVars' PrecLam body'
    in if prec > PrecLam
       then colourParen (pretty ("(" :: String)) <> doc <> colourParen (pretty (")" :: String))
       else doc

  App f arg ->
    let leftDoc = prettyPrec config boundVars PrecApp f
        rightDoc = prettyPrec config boundVars PrecAtom arg
        doc = leftDoc <+> rightDoc
    in if prec > PrecApp
       then colourParen (pretty ("(" :: String)) <> doc <> colourParen (pretty (")" :: String))
       else doc

-- | Collect consecutive lambda abstractions
-- λx. λy. λz. body  →  ([x, y, z], body)
collectLambdas :: Term a -> ([a], Term a)
collectLambdas (Lam x body) =
  let (xs, body') = collectLambdas body
  in (x:xs, body')
collectLambdas term = ([], term)

colourLambda :: Doc AnsiStyle -> Doc AnsiStyle
colourLambda = annotate (color Magenta)

colourBound :: Doc AnsiStyle -> Doc AnsiStyle
colourBound = annotate (color Green)

colourFree :: Doc AnsiStyle -> Doc AnsiStyle
colourFree = annotate (color Yellow)

colourParen :: Doc AnsiStyle -> Doc AnsiStyle
colourParen = annotate (colorDull White)

-- | Render a Doc to plain text
renderPlain :: Doc AnsiStyle -> String
renderPlain doc = T.unpack $ Plain.renderStrict $ layoutPretty defaultLayoutOptions doc

-- | Render a Doc to ANSI coloured text (ANSI codes)
renderColouredString :: Doc AnsiStyle -> String
renderColouredString doc = T.unpack $ renderColouredText doc

-- | Render a Doc to coloured Text (ANSI codes)
renderColouredText :: Doc AnsiStyle -> Text
renderColouredText doc = Term.renderStrict $ layoutPretty defaultLayoutOptions doc
