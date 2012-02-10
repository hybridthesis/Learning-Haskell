import Data.Char (toUpper)
import Data.Tree

quux = Node

data Foo = Bar 
         | Baz String
         deriving Show

data Days = Monday
          | Tuesday
          | Wednesday
          | Thursday
          deriving (Show, Enum)


fooProcessor Bar                  = do putStrLn "ehhh"
fooProcessor (Baz string@(v1:vs)) = do putStrLn $ "Your string is " ++ string
                                       putStrLn $ "Your ending is " ++ vs

myAbs x
  | x < 0     = 0 - x
  | otherwise = x

guessNumber num = do
  putStrLn "Please your guess: "
  guess <- getLine
  case compare (read guess) num of
       LT -> do
         putStrLn "Too low."
         guessNumber num
       GT -> do
         putStrLn "Too High."
         guessNumber num
       EQ -> putStrLn "you're correct!"

main0 = 
  do
    x <- getX
    putStrLn x
    return ()

getX = 
  do
    return "Hello"
    return " world"
    return "!"

isOfName =
  do putStrLn "?"
     userName <- getLine
     case userName of
          "Phil" -> putStrLn "You're awesome"
          "Coen" -> putStrLn "You probably think debugging is great"
          _      -> putStrLn "I don't know you"

capName =
  do putStrLn "What's your name?"
     name <- getLine
     putStrLn $ makeLoud name

makeLoud = map toUpper


{- It's not all that special now is it?
  - No, it is really special
  - I have a new super power
  - Yaay
  -}

myFirstMonad = 
  putStrLn "Umm... hi..." >>
  getLine >>= 
  \name -> putStrLn $ "Hello " ++ name
