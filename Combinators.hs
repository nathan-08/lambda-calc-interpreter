module Combinators
  where

i = "λx.x"
k = "λxy.x"
s = "λxyz.xz(yz)"
--t = "(λx.xy)(λz.y)"
ω = "(λx.xx)(λx.xx)"
y = "λf.(λx.f(xx))(λx.f(xx))"
t = "λϕxy.ϕyx" -- Vertauschungsfunktion
z = "λfgx.f(gx)" -- Zusammensetzungsfunktion
