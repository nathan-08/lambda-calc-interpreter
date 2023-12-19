import Parser (try_parse)
import Lambda (render)
import BetaReduction (β_reduce)

-- evaluate Y combinator on a constant function (will terminate)
yf = "(λf.(λx.f(xx))(λx.f(xx)))λy.a"

parse :: String -> IO ()
parse inp = case try_parse inp of
  Just r  -> putStrLn . render $ β_reduce r
  Nothing -> putStrLn "No result"

-- todo: implement α-equivalence predicate

main :: IO ()
main = putStrLn "run 'ghci Main.hs' and try 'parse \"λxyz.xz(yz)\"'"




