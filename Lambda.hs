module Lambda
  ( Term (..)
  , render
  , fv
  , is_combinator
  , Binding (..)
  , set_bindings
  ) where

import Data.Set
import Data.List (elemIndex)

data Binding = Bound Int
             | Free
     deriving (Show)

data Term = Var Char Binding
          | Abs Char Term
          | App Term Term
     deriving (Show)
 
render :: Term -> String
render v@(Var _ _) = render_var v
render (Abs x t) = ['λ', x, '.'] ++ render t
render (App p q) = enclose p ++ enclose q

render_var :: Term -> String
render_var (Var x (Bound i)) = [x] ++ "[" ++ show i ++ "]"
render_var (Var x Free) = [x] ++ "[-1]"

enclose :: Term -> [Char]
enclose v@(Var _ _) = render_var v
enclose other = "(" ++ render other ++ ")"

fv :: Term -> Set Char -- free variables
fv (Var x _) = singleton x
fv (Abs x t) = delete x (fv t)
fv (App x y) = (fv x) `union` (fv y)

is_combinator :: Term -> Bool
is_combinator = (== 0) . size . fv

set_bindings :: Term -> Term
set_bindings t = ϕ t []
  where ϕ (Var x _) bs = case elemIndex x bs of
          Just i  -> (Var x $ Bound i)
          Nothing -> (Var x Free)
        ϕ (Abs x t) bs = (Abs x (ϕ t (x : bs)))
        ϕ (App p q) bs = App (ϕ p bs) (ϕ q bs)

