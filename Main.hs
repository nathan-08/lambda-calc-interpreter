import Parser (try_parse)
import Lambda (render)
import BetaReduction (β_reduce)

parse :: String -> IO ()
parse inp = case try_parse inp of
  Just r  -> putStrLn . render $ β_reduce r
  Nothing -> putStrLn "No result"

main :: IO ()
main = putStrLn "monadic parser"




