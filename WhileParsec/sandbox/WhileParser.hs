module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Text.Megaparsec hiding (State, parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Data.List (intercalate)
import Debug.Trace

type Num = Integer
type Var = String

type State = Var -> Z

type Z = Integer
type T = Bool
type Pname = String


data Aexp = N Num 
          | V Var 
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
        deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
        deriving (Show, Eq, Read)

data Stm = Skip
         | Ass Var Aexp 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
        deriving (Show, Eq, Read)

statement :: Parser Stm
statement =  try (parens statement)
         <|> try (many (comments) *> whitespace *> compStm <* whitespace)
         <|> try (many (comments) *> whitespace *> stm <* whitespace)

comments :: Parser ()
comments =  (tok "/*" >> manyTill anyChar (tok "*/") >> whitespace >> return ())
        <|> (tok "//" >> manyTill anyChar newline >> whitespace >> return ())

compStm :: Parser Stm
compStm = Comp <$> stm <* tok ";" <* whitespace <*> statement

stm :: Parser Stm
stm =  try (Skip  <$  tok "skip")
   <|> try (Ass   <$> var <* whitespace <* tok ":=" <* whitespace <*> aexp)
   <|> try (If    <$  tok "if" <*> bexp <* tok "then" <*> statement <* tok "else" <*> statement)
   <|> try (While <$  tok "while" <*>  bexp <* tok "do"   <*> statement)
   <|> try (Call  <$  tok "call" <*> pname)
   <|> try (Block <$  tok "begin" <*> (many decv) <*> (many decp) <*> statement <* whitespace <* tok "end")

pname :: Parser Pname
pname = 
  do 
      whitespace
      f <- oneOf(['A' .. 'Z'] ++ ['a' .. 'z'])
      m <-  many(oneOf(['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']))
      whitespace
      return $ ([f] ++ m)

decv :: Parser (Var,Aexp)
decv = 
  do whitespace
     tok "var"
     whitespace
     var <- var
     whitespace
     tok ":="
     whitespace
     expr <- aterm
     tok ";"
     whitespace
     return $ (var, expr)

decp :: Parser (Pname, Stm)
decp = 
  do whitespace
     tok "proc" 
     whitespace
     pro <- pname
     whitespace
     tok "is"
     s <- stm
     tok ";"
     return $ (pro, s)

aexp :: Parser Aexp
aexp =  try (Mult <$> aterm <* whitespace <* tok "*" <* whitespace <*> aterm)
    <|> try (Add  <$> aterm <* whitespace <* tok "+" <* whitespace <*> aterm)
    <|> try (Sub  <$> aterm <* whitespace <* tok "-" <* whitespace <*> aterm)
    <|> aterm                                         

aterm :: Parser Aexp
aterm =  parens aexp
     <|> V <$> var
     <|> N <$> number

bexp :: Parser Bexp
bexp =  try (And <$> bterm <* tok "&" <* whitespace <*> bterm)
    <|> try (Eq  <$> aterm <* tok "=" <* whitespace <*> aterm)
    <|> try (Le  <$> aterm <* tok "<=" <* whitespace <*> aterm)
    <|> try (Neg <$ tok "!" <*> bexp)
    <|> bterm

bterm :: Parser Bexp
bterm =  parens bexp
     <|> boolean

boolean :: Parser Bexp
boolean = do
  s <- tok "true" <|> tok "false"
  if (s == "true")
    then return TRUE
    else return FALSE


var :: Parser Var
var = some(oneOf (['A' .. 'Z'] ++ ['a' .. 'z'])) <* whitespace

relation = (tok "<" >> return Le)
        <|> (tok "=" >> return Eq)

number :: Parser Num
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

whitespace :: Parser ()
whitespace = many (oneOf " \t\n\r") *> pure ()

cr :: Parser [Char]
cr = many (oneOf "\r\n")

tok :: String -> Parser String
tok t = try (whitespace *> string t <* whitespace)

parse :: String -> Stm
parse str =
  case Text.Megaparsec.parse statement "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stm
parseFile file =
  do program  <- readFile file
     case Text.Megaparsec.parse statement "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

-------------------------------------------------------------------------------------

n_val :: Num -> Z
n_val x = x 

a_val :: Aexp -> State -> Z
a_val (N n) s = n_val n
a_val (V v) s = s v
a_val (Mult a1 a2) s = (a_val a1 s) * (a_val a2 s)
a_val (Add a1 a2) s = (a_val a1 s) + (a_val a2 s)
a_val (Sub a1 a2) s = (a_val a1 s) - (a_val a2 s)

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
subst_aexp (Mult a11 a12) v a2 =  Mult (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Add a11 a12) v a2  = Add (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Sub a11 a12) v a2  = Sub (subst_aexp a11 v a2) (subst_aexp a12 v a2) 

update :: State -> Z -> Var -> State
update s v x y
  | x == y    = v
  | otherwise = s y

s' :: State
s' "x" = n_val 5

p :: Stm
p = Comp (Ass "y" (N 1)) (While (Neg (Eq (V "x") (N 1))) (Comp (Ass "y" (Mult (V "y") (V "x"))) (Ass "x" (Sub (V "x") (N 1)))))

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (c, a1, a2) s = if (c s)
                        then (a1 s)
                        else (a2 s)

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass var ax) s = update s (a_val ax s) var
s_ds Skip s = s
s_ds (Comp sm1 sm2) s = (s_ds sm2 (s_ds sm1 s))
s_ds (If b sm1 sm2) s = cond (b_val b, s_ds sm1, s_ds sm2) s
s_ds (While b sm) s = fix ff s where
    ff g = cond (b_val b, g . s_ds sm, id)

------------------------------------------------------------------------------------

s_init :: State
s_init "x" = 0
s_init "y" = 0
s_init "z" = 0
s_init _ = 0

ns_stm :: Config -> Config
ns_stm (Inter (Ass x a) s) = Final (update s (a_val a s) x)
ns_stm (Inter (Skip) s) = Final s
ns_stm (Inter (Comp ss1 ss2) s) = Final s''
  where
      Final s'  = ns_stm (Inter ss1 s)
      Final s'' = ns_stm (Inter ss2 s')
ns_stm (Inter (If b ss1 ss2) s) = case b_val b s of
                                    True      -> Final s' where
                                        Final s' = ns_stm (Inter ss1 s)
                                    otherwise -> Final s' where
                                        Final s' = ns_stm (Inter ss2 s)
ns_stm (Inter (While b ss) s )  = case b_val b s of
                                    True      -> Final s'' where
                                        Final s'  = ns_stm (Inter ss s)
                                        Final s'' = ns_stm (Inter (While b ss) s')
                                    otherwise -> Final s

s_ns ss s = s' where
  Final s' = ns_stm (Inter ss s)

------------------------------------------------------------------------------------


is_Final (Inter ss s) = False
is_Final (Final s)    = True

sos_stm :: Config -> Config
sos_stm (Inter (Ass x a) s) = Final (update s x (a_val a s)) where
    update s x v y = case x == y of
                       True      -> v
                       otherwise -> s y
sos_stm (Inter Skip s) = Final s
sos_stm (Inter (Comp ss1 ss2) s)  = case is_Final(sos_stm (Inter ss1 s)) of
                                      True -> Inter ss2 s' where
                                          Final s' = sos_stm (Inter ss1 s)
                                      otherwise -> Inter (Comp ss1' ss2) s' where
                                          Inter ss1' s' = sos_stm (Inter ss1 s)
sos_stm (Inter (If b ss1 ss2) s) = case b_val b s of
                                     True  -> Inter ss1 s
                                     False -> Inter ss2 s
sos_stm (Inter (While b ss) s) = Inter (If b (Comp ss (While b ss)) Skip) s

deriv_seq (Inter ss s) = (Inter ss s) : (deriv_seq (sos_stm (Inter ss s)))
deriv_seq (Final s) = [Final s]

s_sos ss s = s' where
    Final s' = last (deriv_seq (Inter ss s))

--------------------------------------------------------------------------------------

show_seq fv l = putStrLn (concat (map show_config l)) where
    show_config (Final s) = "\nfinal state:\n" ++ show (map (show_val s) fv)
    show_config (Inter ss s) = "\n" ++ show (map (show_val s) fv)
    show_val s x = " s(" ++x++")="++ show (s x)


-------------------------------------------------------------------------------------

ns_dy :: EnvP -> Config -> Config
ns_dy ep (Inter (Ass x a) s) = Final (update s (a_val a s) x)
ns_dy ep (Inter (Skip) s) = Final s
ns_dy ep (Inter (Comp ss1 ss2) s) = Final s''
  where
      Final s'  = ns_dy ep (Inter ss1 s)
      Final s'' = ns_dy ep (Inter ss2 s')
ns_dy ep (Inter (If b ss1 ss2) s) = case b_val b s of
                                    True      -> Final s' where
                                        Final s' = ns_dy ep (Inter ss1 s)
                                    otherwise -> Final s' where
                                        Final s' = ns_dy ep (Inter ss2 s)
ns_dy ep (Inter (While b ss) s )  = case b_val b s of
                                    True      -> Final s'' where
                                        Final s'  = ns_dy ep (Inter ss s)
                                        Final s'' = ns_dy ep (Inter (While b ss) s')
                                    otherwise -> Final s
ns_dy ep (Inter (Block dv dp sm) s)  = Final s'
  where
      Final s' = ns_dy (upd_pd (dp, ep)) (Inter sm (update_dynamic dv s))
ns_dy ep (Inter (Call pn) s) = Final s'
  where
      p_sm = ep pn
      Final s' = ns_dy ep (Inter p_sm s)


updateEnvp :: EnvP -> Stm -> Pname -> EnvP
updateEnvp e s p y = if(p == y)
                    then s
                    else e y

upd_pd :: (DecP, EnvP) -> EnvP
upd_pd (((p,s):ds), ep) = 
  do ep' <- updateEnvp ep s
     upd_pd (ds, ep')
upd_pd (_, ep) = ep

update_dynamic :: DecV -> State -> State
update_dynamic ((v,a):ds) s = update_dynamic ds (update s (a_val a s) v)
update_dynamic _ s = s



--------------------------------------------------------------------------------------


ns_mx :: EnvP_m  -> Config -> Config
ns_mx ep (Inter (Ass x a) s) = Final (up s x (a_val a s)) where
    up s x v y = case (x == y) of
                   True -> v
                   otherwise -> s y
ns_mx ep (Inter Skip s) = Final s
ns_mx ep (Inter (Comp ss1 ss2) s)  = Final s''
  where
      Final s' = ns_mx ep (Inter ss1 s)
      Final s'' = ns_mx ep (Inter ss2 s')
ns_mx ep (Inter (If b ss1 ss2) s) = case b_val b s of
                                      True  -> Final s' where
                                          Final s' = ns_mx ep (Inter ss1 s)
                                      otherwise -> Final s' where
                                          Final s' = ns_mx ep (Inter ss2 s)
ns_mx ep (Inter (While b ss) s )  = case b_val b s of
                                      True      -> Final s'' where
                                        Final s'  = ns_mx ep (Inter ss s)
                                        Final s'' = ns_mx ep (Inter (While b ss) s')
                                      otherwise -> Final s
ns_mx ep (Inter (Block dv dp sm) s)  = Final s_res where
    s' = update_s s dv
    ep' = upd_pm ep dp
    Final s'' = ns_mx ep' (Inter sm s')
    s_res = (\v -> case (v `elem` (map fst dv)) of
                     True -> s v
                     otherwise ->  s'' v)
ns_mx ep (Inter (Call pn) s) = Final s'
  where
      (p_sm, p_ev) = runM ep pn
      rem = upd_pm p_ev [(pn, p_sm)]
      Final s' = ns_mx rem (Inter p_sm s)


envp_m :: EnvP_m
envp_m = EnvP_m (\pname -> (Skip, envp_m))


upd_pm :: EnvP_m -> DecP -> EnvP_m
upd_pm  ep dp = foldl upd_pm' ep dp
  where
      upd_pm' :: EnvP_m -> (Pname, Stm) -> EnvP_m
      upd_pm' ep (pn, sm) = EnvP_m(\pn' -> case(pn' == pn) of
                                             True      -> (sm, ep)
                                             otherwise -> runM ep pn')

update_s :: State -> DecV -> State
update_s s dv = foldl update_s' s dv
  where
      update_s' :: State -> (Var, Aexp) -> State
      update_s' s (v, ax) = \v' -> case (v == v') of
                                           True -> a_val ax s
                                           otherwise -> s v'

--------------------------------------------------------------------------------------
    
ns_st :: EnvV -> EnvP_s  -> Config -> Config
ns_st ev ep (Inter (Ass x a) s) = Final (up s x (a_val a s)) where
    up s x v y = case (x == y) of
                   True -> v
                   otherwise -> s y
ns_st ev ep (Inter Skip s) = Final s
ns_st ev ep (Inter (Comp ss1 ss2) s)  = Final s''
  where
      Final s' = ns_st ev ep (Inter ss1 s)
      Final s'' = ns_st ev ep (Inter ss2 s')
ns_st ev ep (Inter (If b ss1 ss2) s) = case b_val b s of
                                      True  -> Final s' where
                                          Final s' = ns_st ev ep (Inter ss1 s)
                                      otherwise -> Final s' where
                                          Final s' = ns_st ev ep (Inter ss2 s)
ns_st ev ep (Inter (While b ss) s )  = case b_val b s of
                                      True      -> Final s'' where
                                        Final s'  = ns_st ev ep (Inter ss s)
                                        Final s'' = ns_st ev ep (Inter (While b ss) s')
                                      otherwise -> Final s
ns_st ev ep (Inter (Block dv dp sm) s)  = Final s_res where
    s' = update_s s dv
    ep' = upd_ps ep dp ev
    Final s'' = ns_st ev ep' (Inter sm s')
    s_res = (\v -> case (v `elem` (map fst dv)) of
                     True -> s v
                     otherwise ->  s'' v)
ns_st ev ep (Inter (Call pn) s) = Final s'
  where
      (p_sm, p_ev, p_ep) = runS ep pn
      rem = upd_ps p_ep [(pn, p_sm)] p_ev
      Final s' = ns_st p_ev rem (Inter p_sm s)

envp_s :: EnvP_s
envp_s = EnvP_s (\pname -> (Skip, envv, envp_s))

upd_ps :: EnvP_s -> DecP -> EnvV -> EnvP_s
upd_ps  ep dp ev = foldl upd_ps' ep dp
  where
      upd_ps' :: EnvP_s -> (Pname, Stm) -> EnvP_s
      upd_ps' ep (pn, sm) = EnvP_s(\pn' -> case(pn == pn) of
                                             True      -> (sm, ev, ep)
                                             otherwise -> runS ep pn')


---------------------------------------------------------------------------------------

deriv_seq_d e (Inter ss s) = (Inter ss s) : (deriv_seq_d e (ns_dy e (Inter ss s)))
deriv_seq_d e (Final s) = [Final s]

run_d sm = show_seq ["x", "y"] (deriv_seq_d envp (Inter sm s_init))

s_drun sm s = s' where
    Final s' = last (deriv_seq_d envp (Inter sm s))

-----

deriv_seq_m e (Inter ss s) = (Inter ss s) : (deriv_seq_m e (ns_mx e (Inter ss s)))
deriv_seq_m e (Final s) = [Final s]

run_m sm = show_seq ["x", "y"] (deriv_seq_m envp_m (Inter sm s_init))

s_mrun sm s = s' where
    Final s' = last (deriv_seq_m envp_m (Inter sm s))

-----

deriv_seq_s v e (Inter ss s) = (Inter ss s) : (deriv_seq_s v e (ns_st v e (Inter ss s)))
deriv_seq_s v e (Final s) = [Final s]

run_s sm = show_seq ["x", "y"] (deriv_seq_s envv envp_s (Inter sm s_init))

s_srun sm s = s' where
    Final s' = last (deriv_seq_s envv envp_s (Inter sm s))


scopeProg = parse "//scope test (p.53)\nbegin\nvar x:=0;\nproc p is x:=x*2;\nproc q is call p;\nbegin\nvar x:=5;\nproc p is x:=x+1;\n(call q;\ny:=x)\nend\nend"


---------------------------------------------------------------------------------------

type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

data Config = Inter Stm State | Final State

type Loc = Num
type EnvV = Var -> Loc

type EnvP = Pname -> Stm

newtype EnvP_m = EnvP_m {runM :: Pname -> (Stm, EnvP_m)}
newtype EnvP_s = EnvP_s {runS :: Pname -> (Stm, EnvV, EnvP_s)}

new :: Loc -> Loc
new l = l + 1

s :: State
s "x" = 0
s "y" = 0
s "z" = 0
s var = 0

envp :: EnvP
envp "p" = Skip
envp _ = Skip

envv :: EnvV
envv "p" = 1
envv _ = 0


s_dynamic :: Stm -> State -> State
s_dynamic sm s = s_drun sm s

s_mixed :: Stm -> State -> State
s_mixed sm s = s_mrun sm s

s_static :: Stm -> State -> State
s_static sm s = s_srun sm s

