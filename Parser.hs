module Parser (try_parse) where

import Control.Applicative
import Data.Char
import Lambda (Term(..), render, fv, is_combinator, Binding(..), set_bindings)
newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
             []     -> []
             (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

symbol :: Parser Char
symbol = sat (\x -> isAlpha x && x /= 'λ')

var :: Parser Term
var = do x <- symbol
         return (Var x Free) -- binding pointer uninitialized

abst' :: Parser Term -- allow for λxyz ≡ (λx(λy(λz)))
abst' = do char 'λ'
           xs <- some symbol
           char '.'
           t <- term
           return $ foldr (\s tt -> Abs s tt) t xs

app' :: Parser Term -- allow for MABC ≡ (((MA)B)C)
app' = do t1 <- concrete_term
          t2 <- concrete_term
          ts <- many concrete_term
          return $ foldl (\tt s -> App tt s) t1 (t2 : ts)

enclosed_term :: Parser Term -- char '(' >>= term >>= char ')'
enclosed_term = do char '('
                   t <- term
                   char ')'
                   return t

term :: Parser Term
term = app' <|> concrete_term

concrete_term = abst' <|> var <|> enclosed_term

bound_term :: Parser Term
bound_term = do t <- term
                return $ set_bindings t

i = "λx.x"
k = "λxy.x"
s = "λxyz.xz(yz)"
t = "(λx.xy)(λz.y)"
ω = "(λx.xx)(λx.xx)"
y = "λf.(λx.f(xx))(λx.f(xx))"

-- extraction functions --

try_parse :: String -> Maybe Term
try_parse inp = extract $ parse bound_term inp

print_term :: [(Term, String)] -> IO ()
print_term [] = putStrLn "No result"
print_term ((t,_) : _) = putStrLn . render $ t

extract :: [(Term, String)] -> Maybe Term
extract [] = Nothing
extract ((x,_):_) = Just x

extract' :: [(Term, String)] -> Term
extract' [] = error "no term"
extract' ((x,_):_) = x

fv' = fv . extract'

-------------------------------

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                 []         -> []
                 [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                    []         -> []
                    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = P (\inp -> case parse px inp of
                   []        -> []
                   [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py = P (\inp -> case parse px inp of
                    []        -> parse py inp
                    [(v,out)] -> [(v,out)])
-- unused

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
