import Prelude hiding (Num)
import qualified Prelude (Num)
type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = N Num | V Var | Mult Aexp Aexp |
             Add Aexp Aexp | Sub Aexp Aexp
              deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE | Neg Bexp | And Bexp Bexp |
             Eq Aexp Aexp | Le Aexp Aexp
              deriving (Show, Eq, Read)

n_val :: Num -> Z
n_val x = x 

s :: State
s "x" = n_val 1
s "y" = n_val 2
s "z" = n_val 3
s var = n_val 0

a :: Aexp
a = Mult (Add (V "x") (V "v")) (Sub (V "z") (N 1))

a_val :: Aexp -> State -> Z
a_val (N n) s = n_val n
a_val (V v) s = s v
a_val (Mult a1 a2) s = (a_val a1 s) * (a_val a2 s)
a_val (Add a1 a2) s = (a_val a1 s) + (a_val a2 s)
a_val (Sub a1 a2) s = (a_val a1 s) - (a_val a2 s)

b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

b_val :: Bexp -> State -> T
b_val TRUE s = True
b_val FALSE s = False
b_val (Neg b) s = case b_val b s of 
                    True -> False
                    False -> True
b_val (And b1 b2) s = case (b1, b2) of
                        (TRUE, TRUE) -> True
                        otherwise -> False
b_val (Eq a1 a2) s = if (a_val a1 s) == (a_val a2 s)
                        then True
                        else False
b_val (Le a1 a2) s = if (a_val a1 s) <= (a_val a2 s)
                            then True
                            else False 
