module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

newtype WordList
  = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed incorrectGuesses) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ guessed
      ++ ", incorrect: "
      ++ show incorrectGuesses

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _ _) ch = ch `elem` str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) ch = ch `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s incorrectGuesses) c =
  Puzzle word newFilledInSoFar (c : s) newIncorrectGuesses
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
    newIncorrectGuesses = if newFilledInSoFar == filledInSoFar then incorrectGuesses + 1 else incorrectGuesses

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed incorrectGuesses) =
  when (incorrectGuesses > 7)
    $ do putStrLn "You lose!"
         putStrLn $ "The word was: " ++ wordToGuess
         exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  when (all isJust filledInSoFar)
    $ do putStrLn "You win!"
         exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
