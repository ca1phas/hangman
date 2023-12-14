import System.IO

sgetChar :: IO Char
sgetChar = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

sgetLine :: IO String
sgetLine = do
  x <- sgetChar
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

match :: String -> String -> String
match ws gs = [if w `elem` gs then w else '-' | w <- ws]

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "Perfect match!"
    else do
      putStrLn (match word guess)
      play word

hangman :: IO ()
hangman = do
  putStr "Think of a word: "
  word <- sgetLine
  putStr "Try to guess it: "
  play word