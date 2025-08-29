{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Random.Stateful
import Control.Monad (foldM)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

type Chain = M.Map T.Text [T.Text]

startTok :: T.Text
startTok = "<START>"

endTok :: T.Text
endTok = "<END>"

-- Tokenize: lowercase, split on whitespace, separate punctuation
tokenize :: T.Text -> [T.Text]
tokenize text = concatMap splitPunctuation words
  where
    words = T.words (T.toLower text)
    
    splitPunctuation :: T.Text -> [T.Text]
    splitPunctuation word = go word []
      where
        go w acc
          | T.null w = reverse acc
          | otherwise = 
              let (prefix, rest) = T.span isNotPunct w
                  (punct, remainder) = T.span isPunct rest
              in if T.null prefix
                 then if T.null punct
                      then reverse acc
                      else go remainder (splitIntoPunctChars punct ++ acc)
                 else go rest (splitIntoPunctChars punct ++ [prefix] ++ acc)
        
        isPunct c = c `elem` (".,;:?!" :: String)
        isNotPunct c = not (isPunct c)
        
        splitIntoPunctChars :: T.Text -> [T.Text]
        splitIntoPunctChars t = map T.singleton (T.unpack t)

-- Add sentence boundary markers
addSentenceMarkers :: [T.Text] -> [T.Text]
addSentenceMarkers tokens = startTok : go tokens []
  where
    go [] acc = reverse (endTok : acc)
    go (t:ts) acc
      | t `elem` [".", "?", "!"] = 
          if null ts
          then reverse (endTok : t : acc)
          else go ts (startTok : endTok : t : acc)
      | otherwise = go ts (t : acc)

-- Build the bigram chain
buildChain :: [T.Text] -> Chain
buildChain tokens = go tokens M.empty
  where
    go [] chain = chain
    go [_] chain = chain
    go (t1:t2:ts) chain = 
      let updatedChain = M.insertWith (++) t1 [t2] chain
      in go (t2:ts) updatedChain

-- Get next word from chain
nextWord :: StatefulGen g m => g -> Chain -> T.Text -> m (Maybe T.Text)
nextWord gen chain currentWord =
  case M.lookup currentWord chain of
    Nothing -> return (Just endTok)
    Just possibleWords -> do
      idx <- uniformRM (0, length possibleWords - 1) gen
      return (Just (possibleWords !! idx))

-- Generate text
generate :: StatefulGen g m => g -> Chain -> Int -> m [T.Text]
generate gen chain maxLength = go startTok [] 0
  where
    go currentWord acc count
      | count >= maxLength = return (reverse acc)
      | currentWord == endTok = return (reverse acc)
      | currentWord == startTok = do
          maybeNext <- nextWord gen chain currentWord
          case maybeNext of
            Nothing -> return (reverse acc)
            Just next -> go next acc (count + 1)
      | otherwise = do
          maybeNext <- nextWord gen chain currentWord
          case maybeNext of
            Nothing -> return (reverse (currentWord : acc))
            Just next -> go next (currentWord : acc) (count + 1)

-- Detokenize: join with spaces, attach punctuation
detokenize :: [T.Text] -> T.Text
detokenize tokens = T.strip (go tokens T.empty True)
  where
    go [] result _ = result
    go (t:ts) result startOfSentence
      | t == startTok = go ts result True
      | t == endTok = go ts result True
      | isPunctuation t = go ts (result <> t) False
      | startOfSentence = 
          let capitalized = capitalizeFirst t
          in go ts (result <> capitalized) False
      | otherwise = go ts (result <> " " <> t) False
    
    isPunctuation t = T.length t == 1 && T.head t `elem` (".,;:?!" :: String)
    
    capitalizeFirst t = 
      case T.uncons t of
        Nothing -> t
        Just (c, rest) -> T.cons (toUpper c) rest

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: markov <file> [length]"
    (filename:rest) -> do
      let maxLen = case listToMaybe rest >>= readMaybe of
                     Just n -> n
                     Nothing -> 50
      
      text <- T.readFile filename
      let tokens = addSentenceMarkers (tokenize text)
      let chain = buildChain tokens
      
      gen <- newIOGenM =<< newStdGen
      generatedTokens <- generate gen chain maxLen
      T.putStrLn (detokenize generatedTokens)