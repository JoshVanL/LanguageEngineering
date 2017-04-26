module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Data.List (intercalate)

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
        deriving (Show)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
        deriving (Show)

data Stm = Skip
         | Ass Var Aexp 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
        deriving (Show)

statement :: Parser Stm
statement =  try (parens statement)
         <|> try (many (comments) *> compStm)
         <|> try (many (comments) *> stm)

comments :: Parser ()
comments =  (tok "/*" >> manyTill anyChar (tok "*/") >> whitespace >> return ())
        <|> (tok "//" >> manyTill anyChar newline >> whitespace >> return ())

compStm :: Parser Stm
compStm = Comp <$> stm <* tok ";" <* whitespace <*> statement

stm :: Parser Stm
stm =  try (Skip  <$  tok "skip")
   <|> try (Ass   <$> var <* tok ":="   <*> aexp)
   <|> try (If    <$  tok "if" <*> bexp <* tok "then" <*> statement <* tok "else" <*> statement)
   <|> try (While <$  tok "while" <*>  bexp <* tok "do"   <*> statement)
   <|> try (Call  <$  tok "call" <*> pname)
   <|> try (Block <$  tok "begin" <*> (many decv) <*> (many decp) <*> statement <* tok "end")

pname :: Parser Pname
pname = some(oneOf (['A' .. 'Z'] ++ ['a' .. 'z'])) <* whitespace

decv :: Parser (Var,Aexp)
decv = 
  do tok "var"
     var <- var
     tok ":="
     expr <- aterm
     tok ";"
     return $ (var, expr)

decp :: Parser (Pname, Stm)
decp = 
  do tok "proc" 
     pro <- pname
     tok "is"
     s <- stm
     tok ";"
     return $ (pro, s)

aexp :: Parser Aexp
aexp =  try (Mult <$> aterm <* tok "*" <*> aterm)
    <|> try (Add  <$> aterm <* tok "+" <*> aterm)
    <|> try (Sub  <$> aterm <* tok "-" <*> aterm)
    <|> aterm

aterm :: Parser Aexp
aterm =  parens aexp
     <|> V <$> var
     <|> N <$> number

bexp :: Parser Bexp
bexp =  try (And <$> bterm <* tok "&" <*> bterm)
    <|> try (Eq  <$> aterm <* tok "=" <*> aterm)
    <|> try (Le  <$> aterm <* tok "<=" <*> aterm)
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
tok t = try (string t <* whitespace)

parseString :: String -> Stm
parseString str =
  case parse statement "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stm
parseFile file =
  do program  <- readFile file
     case parse statement "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

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

s :: State
s "x" = n_val 5
s "y" = n_val 9
s "z" = n_val 5
s var = n_val 0

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

new :: Loc -> Loc
new l = l + 1

envp :: EnvP
envp "p" = Skip
envp "q" = Skip
envp var = Skip

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

s_dn :: EnvP -> Stm -> State -> State
s_dn ep (Ass var ax) s = update s (a_val ax s) var
s_dn ep Skip s = s
s_dn ep (Comp sm1 sm2) s = (s_dn ep sm2 (s_dn ep sm1 s))
s_dn ep (If b sm1 sm2) s = cond (b_val b, s_dn ep sm1, s_dn ep sm2) s
s_dn ep (While b sm) s = fix ff s where
    ff g = cond (b_val b, g . s_dn ep sm, id)
s_dn ep (Block dv dp sm) s  = s_dn (upd_pd (dp, ep)) sm (update_dynamic dv s)
s_dn ep (Call pn) s = s_dn ep (ep pn) s

s_dynamic :: Stm -> State -> State
s_dynamic sm s = s_dn envp sm s

-------------------

s_mx :: EnvP_m -> Stm -> State -> State
s_mx ep (Ass var ax) s = update s (a_val ax s) var
s_mx ep Skip s = s
s_mx ep (Comp sm1 sm2) s = (s_mx ep sm2 (s_mx ep sm1 s))
s_mx ep (If b sm1 sm2) s = cond (b_val b, s_mx ep sm1, s_mx ep sm2) s
s_mx ep (While b sm) s = fix ff s where
    ff g = cond (b_val b, g . s_mx ep sm, id)
--s_mx ep (Block dv dp sm) s  = s_mx (upd_pm ep dp) sm (update_s s dv)
s_mx ep (Block dv dp sm) s  = s_restored
  where
      s'  = update_s s dv
      ep' = upd_pm ep dp
      s'' = s_mx ep' sm s'
      s_restored = (\v -> if (v `elem` (map fst dv)) then s v else s'' v)
s_mx ep (Call pn) s = s'
  where
      (p_sm, p_ev) = run ep pn
      rem = upd_pm p_ev [(pn, p_sm)]
      s' = s_mx rem p_sm s

--
--s_dn ep (Block dv dp sm) s  = s_dn (upd_pd (dp, ep)) sm (update_dynamic dv s)
--s_dn ep (Call pn) s = s_dn ep (ep pn) s

s_mixed :: Stm -> State -> State
s_mixed sm s = s_mxd envp sm s

--s_mxd e sm s = s_mx (EnvP_m(e)) sm s

--envps :: EnvP_m
--envps x = (EnvP_m(envp x))



updGen :: Eq a => (a -> b) -> b -> a -> (a -> b)
updGen en s p = (\x -> if p == x then s else en x)

update_s :: State -> DecV -> State
update_s s decV = foldl update_s' s decV
  where
      update_s' :: State -> (Var, Aexp) -> State
      update_s' s (var, aexp) = \var' -> case () of
               _ | var' == var -> a_val aexp s
                 | otherwise   -> s var'

upd_pm :: EnvP_m -> DecP -> EnvP_m
upd_pm  envP decP = foldl upd_pm' envP decP
  where
      upd_pm' :: EnvP_m -> (Pname, Stm) -> EnvP_m
      upd_pm' envP (pname, stm) = EnvP_m(\pname' -> case() of
        _ | pname' == pname -> (stm, envP)
          | otherwise       -> run envP pname')

type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]

type Loc = Num
type EnvV = Var -> Loc

type EnvP = Pname -> Stm
--type EnvPs = Pname -> (Stm, EnvP_s)
--data EnvP_s = EnvP 
--            | EnvPs
newtype EnvP_m = EnvP_m {run :: Pname -> (Stm, EnvP_m)}
--s_mixed :: Stm -> State -> State
--

--updateEnvps :: EnvPs -> Stm -> Pname -> EnvPs
--updateEnvps e1 s p y = if(p == y)
--                    then (s, e1)
--                    else e1 y
--
--extract :: Pname -> (Stm, EnvP_s) -> EnvP_s
--extract p (s, e) = e


--updateEnvp :: EnvP -> Stm -> Pname -> EnvP
--updateEnvp e s p y = if(p == y)
--                    then s
--                    else e y

--updateEnvp_s :: EnvP_s -> Stm -> Pname -> EnvP_s
--updateEnvp_s e s p y = updateEnvp e s p y

--upd_pd :: (DecP, EnvP) -> EnvP
--upd_pd (((p,s):ds), ep) = 
--  do ep' <- updateEnvp ep s
--     upd_pd (ds, ep')
--upd_pd (_, ep) = ep

--upd_pm :: (DecP, EnvP_s) -> EnvP_s
--upd_pm (((p,s):ds), ep) =
--  do ep' <- EnvP_s (updGen s ep)
--     upd_pm (ds, ep')
--upd_pm (_, ep) = ep

--updateEnvp_s :: EnvP_s -> Stm -> Pname -> EnvP_s
--updateEnvp_s e s p y = if(p == y)
--                    then s
--                    else e y
--
