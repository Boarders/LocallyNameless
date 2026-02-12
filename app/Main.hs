{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.Haskeline
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.State
import Control.Exception (catch, SomeException, displayException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (runParser, errorBundlePretty)

import LN.Expression (Term(..), nf, whnf)
import LN.Parser (parseExpr)
import LN.PrettyPrint

data ReplState = ReplState
  { replConfig :: PrettyConfig
  , replEvalMode :: EvalMode
  , replSupportsColor :: Bool
  }

data EvalMode = NormalForm | WeakHeadNormalForm
  deriving (Eq, Show)

initialState :: Bool -> ReplState
initialState supportsColor = ReplState
  { replConfig = defaultConfig
  , replEvalMode = NormalForm
  , replSupportsColor = supportsColor
  }

-- | Haskeline settings
haskelineSettings :: Settings IO
haskelineSettings = Settings
  { historyFile = Just ".locallynameless_history"
  , complete = noCompletion
  , autoAddHistory = True
  }

welcomeMessage :: String
welcomeMessage = unlines
  [ "Locallynameless Lambda Calculus REPL"
  , "Type :help for help, :quit to exit"
  ]

main :: IO ()
main = do
  supportsColor <- hSupportsANSI stdout
  putStrLn welcomeMessage
  runInputT haskelineSettings (evalStateT replLoop (initialState supportsColor))
  putStrLn "Au Revoir!"

replLoop :: StateT ReplState (InputT IO) ()
replLoop = do
  minput <- lift $ getInputLine "λ> "
  case minput of
    Nothing -> return ()  -- Ctrl-D exits
    Just input -> do
      continue <- if null input || all (== ' ') input
                  then return True
                  else handleInput (T.pack input)
      when continue replLoop

handleInput :: Text -> StateT ReplState (InputT IO) Bool
handleInput input
  | T.isPrefixOf ":" input = handleCommand input
  | otherwise = handleExpression input >> pure True

handleCommand :: Text -> StateT ReplState (InputT IO) Bool
handleCommand cmd = case T.words cmd of
  [":quit"] -> pure False
  [":q"]    -> pure False

  [":help"] -> liftIO (putStrLn helpText) >> pure True
  [":h"]    -> liftIO (putStrLn helpText) >> pure True
  [":?"]    -> liftIO (putStrLn helpText) >> pure True

  [":ascii"] -> do
    modify (\s -> s { replConfig = asciiConfig })
    lift $ outputStrLn "Switched to ASCII mode (\\)"
    pure True

  [":unicode"] -> do
    modify (\s -> s { replConfig = defaultConfig })
    lift $ outputStrLn "Switched to Unicode mode (λ)"
    pure True

  (":nf":rest) -> do
    modify (\s -> s { replEvalMode = NormalForm })
    if null rest
      then lift $ outputStrLn "Evaluation mode set to normal form"
      else handleExpression (T.unwords rest) >> pure ()
    pure True

  (":whnf":rest) -> do
    modify (\s -> s { replEvalMode = WeakHeadNormalForm })
    if null rest
      then lift $ outputStrLn "Evaluation mode set to weak head normal form"
      else handleExpression (T.unwords rest) >> pure ()
    pure True

  (":parse":rest) -> handleParse (T.unwords rest) >> pure True

  _ -> lift (outputStrLn "Unknown command. Type :help for help.") >> pure True

handleExpression :: Text -> StateT ReplState (InputT IO) ()
handleExpression input = do
  state <- get
  case runParser parseExpr "" input of
    Left err -> lift $ outputStrLn $ "Parse error:\n" ++ errorBundlePretty err
    Right term -> do
      result <- liftIO $ catch
        (let evaluated = case replEvalMode state of
               NormalForm -> nf term
               WeakHeadNormalForm -> whnf term
         in pure (Just evaluated))
        (\(e :: SomeException) -> do
           putStrLn $ "Evaluation error: " ++ displayException e
           pure Nothing)
      case result of
        Just evaluated -> do
          let doc = prettyTermDocWith (replConfig state) evaluated
              output = if replSupportsColor state
                       then renderColouredText doc
                       else T.pack $ renderPlain doc
          liftIO $ TIO.putStrLn output
        Nothing -> pure ()

handleParse :: Text -> StateT ReplState (InputT IO) ()
handleParse input = do
  state <- get
  case runParser parseExpr "" input of
    Left err -> lift $ outputStrLn $ "Parse error:\n" ++ errorBundlePretty err
    Right term -> do
      lift $ outputStrLn $ "Parsed AST:"
      lift $ outputStrLn $ show term
      lift $ outputStrLn ""
      lift $ outputStrLn "Pretty printed:"
      let doc = prettyTermDocWith (replConfig state) term
          output = if replSupportsColor state
                   then renderColouredText doc
                   else T.pack $ renderPlain doc
      liftIO $ TIO.putStrLn output

helpText :: String
helpText = unlines
  [ "Lambda Calculus REPL"
  , ""
  , "Commands:"
  , "  :quit, :q       Exit the REPL"
  , "  :help, :h, :?   Show this help"
  , "  :nf [expr]      Set normal form mode or evaluate expression"
  , "  :whnf [expr]    Set WHNF mode or evaluate expression"
  , "  :parse <expr>   Parse and show AST without evaluating"
  , "  :ascii          Use ASCII lambda (\\)"
  , "  :unicode        Use Unicode lambda (λ)"
  , ""
  , "Syntax:"
  , "  Variables:      x, y, foo"
  , "  Lambda:         \\x. x  or  \\x y. x"
  , "  Application:    f x  or  f x y"
  , "  Parentheses:    (\\x. x) y"
  , ""
  , "Examples:"
  , "  \\x. x                    -- Identity function"
  , "  \\x y. x                  -- const (K combinator)"
  , "  \\s z. s (s z)            -- Church numeral 2"
  ]
