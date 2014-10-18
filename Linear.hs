module Linear (
 Q(..), T(..), PTy(..), Ty,
 Context(..), append, difference
 ) where

import Prelude hiding (elem)

data Q
 = Lin
 | Un
 deriving (Eq, Show)

instance Ord Q where
 Lin <= Lin = True
 Lin <= Un = True
 Un <= Un = True
 _ <= _ = False

data T
 = Var String
 | Boole Q Bool
 | IfThenElse T T T
 | Pair Q T T
 | Split T String String T
 | Lambda Q String Ty T
 | T :$ T
 deriving (Eq, Show)

data PTy
 = BooleTy
 | Ty :* Ty
 | Ty :-> Ty
 deriving (Eq, Show)

type Ty = (Q, PTy)

data Context
 = Empty | Context ::: (String, Ty)
 deriving (Eq, Show)

c `append` Empty = c
c `append` (d ::: t) = (c `append` d) ::: t

difference :: Context -> Context -> Maybe Context
c `difference` Empty = Just c
c `difference` (d ::: x@(s, (Lin, t))) = case c `difference` d of
    Just d' -> if x `elem` d' then Nothing else Just d'
    Nothing -> Nothing
c `difference` (d ::: x@(s, (Un, t))) = fmap (delete x) (c `difference` d)

e `elem` Empty = False
e `elem` (cs ::: c) = if e == c then True else e `elem` cs

--delete :: (String, Ty) -> Context -> Context
delete e Empty = Empty
delete e (cs ::: c) = if e == c then cs else (delete e cs) ::: c