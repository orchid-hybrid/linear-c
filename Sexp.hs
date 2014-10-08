module Sexp (
 ) where

import Data.List

import Syntax

sexp' l = "(" ++ intercalate " " l ++ ")"
class Sexp a where sexp :: a -> String
instance Sexp Bool where
 sexp True = "#t"
 sexp False = "#f"
instance Sexp Q where
 sexp Lin = "lin"
 sexp Un = "un"
instance Sexp Term where
 sexp (Variable s) = s
 sexp (Boolean (q,b)) = sexp' [sexp q, sexp b]
 sexp (If b t e) = sexp' ["if", sexp b, sexp t, sexp e]
 sexp (Pair (q, (t1, t2))) = sexp' [sexp q, sexp' ["pair", sexp t1, sexp t2]]
 sexp (Split t (v1, v2) m) = sexp' ["split", sexp' [sexp' [v1, v2], sexp t], sexp m]
 sexp (Lambda (q, (v, t, m))) = sexp' [sexp q, sexp' ["lambda", sexp' [v, sexp t], sexp m]]
 sexp (Application m n) = sexp' ["app", sexp m, sexp n]
instance Sexp P where
 sexp Bool_T = "bool"
 sexp (t1 :*: t2) = sexp' ["*", sexp t1, sexp t2]
 sexp (t1 :->: t2) = sexp' ["->", sexp t1, sexp t2]
instance (Sexp p1, Sexp p2) => Sexp (p1,p2) where
 sexp (x,y) = sexp' [sexp x, sexp y]

test1 = Application (Variable "f") (Variable "x")
test2 = If (Boolean (Lin, True))
           (Pair (Lin, (Variable "x",Variable "y")))
           (Pair (Un, (Variable "y",Variable "x")))
test3 = Lambda (Lin, ("xy", (Lin,((Un, Bool_T) :*: (Un,Bool_T))),
          Split (Variable "xy") ("x", "y")
           (Pair (Lin, (Variable "y", Variable "x")))))

