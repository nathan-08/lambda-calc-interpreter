module BetaReduction (β_reduce) where

import Lambda (Term(..), Binding(..))

β_reduce :: Term -> Term
β_reduce v@(Var _ _) = v
β_reduce (Abs x t) = Abs x (β_reduce t)
β_reduce (App (Abs _ lh) rh) = β_reduce new_lh
  where new_lh = subst lh rh 0
        subst (Abs x t)   graft d = Abs x (subst t graft (d+1))
        subst (App t1 t2) graft d = App (subst t1 graft d) (subst t2 graft d)
        subst v@(Var _ Free) _ _  = v
        subst v@(Var x (Bound i)) graft d
          | i == d = update graft d
          | i <  d = v
          | i >  d = (Var x (Bound (i-1)))
β_reduce (App t1 t2) = β_reduce (App (β_reduce t1) (β_reduce t2))

update :: Term -> Int -> Term
-- update bindings of bound variables
update graft d = ϕ graft 0
  where ϕ (Abs x t) i = Abs x (ϕ t (i+1))
        ϕ (App t1 t2) i = App (ϕ t1 i) (ϕ t2 i)
        ϕ v@(Var _ Free) _ = v
        ϕ v@(Var x (Bound b)) i
          | b < i     = v
          | otherwise = Var x (Bound (b+d))
          
