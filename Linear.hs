module Linear (
 Q(..), T(..), PTy(..), Ty,
 Context(..), append
 ) where

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

