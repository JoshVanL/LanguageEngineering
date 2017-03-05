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
            Eq Aexp Aexp | Le Aexp Aexp | Imp Bexp Bexp
              deriving (Show, Eq, Read)

n_val :: Num -> Z
n_val x = x 

s :: State
s "x" = n_val 1
s "y" = n_val 2
s "z" = n_val 3
s var = n_val 0

a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

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
b_val (And b1 b2) s = case ((b_val b1 s), (b_val b2 s)) of 
                        (True, True) -> True
                        otherwise -> False
b_val (Eq a1 a2) s = if (a_val a1 s) == (a_val a2 s)
                        then True
                        else False
b_val (Le a1 a2) s = if (a_val a1 s) <= (a_val a2 s)
                        then True
                        else False 
b_val (Imp b1 b2) s = case b_val b1 s of
                        True -> case b_val b2 s of
                                  True -> True
                                  False -> False 
                        False -> True

fv_aexp :: Aexp -> [Var]
fv_aexp (V v) = [v]
fv_aexp (N n) = []
fv_aexp (Mult a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)
fv_aexp (Add a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)
fv_aexp (Sub a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)

subst_aexp :: Aexp -> Var -> Aexp -> Aexp
subst_aexp (V var) v a2 = if (var == v)
                             then a2
                             else V var
subst_aexp (N n) v a2 = N n
subst_aexp (Mult a11 a12) v a2 = Mult (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Add a11 a12) v a2 = Add (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Sub a11 a12) v a2 = Sub (subst_aexp a11 v a2) (subst_aexp a12 v a2) 


data Stm = Ass Var Aexp | Skip | Comp Stm Stm | 
            If Bexp Stm Stm | While Bexp Stm
            deriving (Show, Eq, Read)

p :: Stm
p = Comp (Ass "y" (N 1)) 
    (While 
        (Neg (Eq (V "x") (N 2)))
        (Comp
        (Ass "y" (Mult (V "y") (V "x")))
        (Ass "x" (Sub (V "x") (N 1)))))

update :: State -> Z -> Var -> State
update s i v y = if(v == y) 
                    then i
                    else s y

s' :: State
s' "x" = n_val 5

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (c, a1, a2) s = if (c s)
                        then (a1 s)
                        else (a2 s)

fix :: ((State -> State) -> (State -> State)) -> (State->State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass var ax) s = update s (a_val ax s) var
s_ds Skip s = s
s_ds (Comp sm1 sm2) s = (s_ds sm2 (s_ds sm1 s))
s_ds (If b sm1 sm2) s = cond (b_val b, s_ds sm1, s_ds sm2) s
s_ds (While b sm) s = fix ff s where
    ff g = cond (b_val b, g . s_ds sm, id)

