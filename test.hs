module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let arg n = args !! n in
    putStrLn ("Hello, " ++ arg 0 ++ arg 1)
