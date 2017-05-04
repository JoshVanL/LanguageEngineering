module ProcCompiler where
import Data.Functor
import Data.Monoid((<>))
import Data.Functor.Identity
import Control.Monad
import qualified Control.Monad.State  as S
import Control.Monad.State hiding (State)
import Control.Monad.Fix

import qualified Data.Map.Strict as M

import Text.Parsec hiding(parse,runParser,State)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language(javaStyle)

import qualified Prelude
import Prelude hiding(Num)

-- Given abstract syntax

type Num = Integer
type Var = String
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]


data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
          deriving Show

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Le Aexp Aexp
          | Eq Aexp Aexp
          deriving Show

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
         | AuxBlockEnvRes EnvV EnvP
         | AuxProcEnvRes EnvV EnvP
         deriving Show

type Loc = Num
type T = Bool
type Z = Integer
type State = Var -> Maybe Z

data Store = Store { mappings::M.Map Loc Z
                   , next::Loc} deriving Show

type EnvV = M.Map Var Loc

newtype EnvP = EnvProc {procMappings::M.Map Pname (Stm,EnvV,EnvP)} deriving Show
instance Monoid EnvP where
 mappend  e1 e2 = EnvProc$ mappend (procMappings e1) (procMappings e2)
 mempty = EnvProc mempty


new::Loc->Loc
new =succ


getVal::EnvV->Store->Var->Maybe Z
getVal envV sto = \v-> M.lookup v envV >>= flip M.lookup (mappings sto)

cond::(Store->T)->(Store->Store)->(Store->Store)->(Store->Store)
cond p g1 g2 s = case p s of
                  True -> g1 s
                  _    -> g2 s


a_eval::Aexp->EnvV->Store->Maybe Z
a_eval aexp envV sto =case aexp of
 N n        -> return n
 V x        -> getVal envV sto x
 Mult a1 a2 -> liftM2 (*) (a_eval a1 envV sto) (a_eval a2 envV sto)
 Add a1 a2  -> liftM2 (+) (a_eval a1 envV sto) (a_eval a2 envV sto)
 Sub a1 a2  -> liftM2 (-) (a_eval a1 envV sto) (a_eval a2 envV sto)

b_eval::Bexp->EnvV->Store->Maybe T
b_eval bexp envV sto =case bexp of
 TRUE -> return True
 FALSE -> return False
 Neg b -> liftM not $ b_eval b envV sto
 And b1 b2 -> liftM2 (&&) (b_eval b1 envV sto) (b_eval b2 envV sto)
 Le a1 a2  -> liftM2 (<=) (a_eval a1 envV sto) (a_eval a2 envV sto)
 Eq a1 a2  -> liftM2 (==) (a_eval a1 envV sto) (a_eval a2 envV sto)

--given a Loc and a val Z updateSto inserts a pair (Loc,Z) and updates next component
updateSto::Loc->Z->Store->Store
updateSto loc z sto = sto{mappings = M.insert loc z oldMappings,next = new l}
 where
  l = next sto
  oldMappings = mappings sto

--given a Var and a Loc updateEnvV inserts a pair (Var,Loc)
updateEnvV::Var->Loc->EnvV->EnvV
updateEnvV=M.insert

-- given a procName bodyStatements variable-environment procedureEnvironment
-- updateEnvP updates the procedureEnvironment by inserting a pair
updateEnvP::Pname->Stm->EnvV->EnvP->EnvP
updateEnvP pname stm envV envP = envP { procMappings=M.insert pname (stm,envV,envP) (procMappings envP)}

updateDv::DecV->(EnvV,Store)->Maybe (EnvV,Store)
updateDv [] x = return x
updateDv ((var,aexp):dv) (envV,sto) = do
    a_eval aexp envV sto >>= \val-> let l = next sto
                                        newEnv=updateEnvV var l envV
                                        newSto=updateSto l val sto
                                    in return (newEnv,newSto) >>= updateDv dv

updateDp::DecP->EnvV->EnvP->Maybe EnvP
updateDp [] _ envP =return envP
updateDp ((pname,stm):dp) envV envP = return (updateEnvP pname stm envV envP) >>= updateDp dp envV

u=undefined

type SemanticFunction = Stm->EnvV->EnvP->Store->Store


getVarLoc::Var->EnvV->Maybe Loc
getVarLoc = M.lookup

type Mystate =(EnvV,EnvP,Store)

data Configuration = Configuration {statements::[Stm],varEnv::EnvV,procEnv::EnvP,store::Store, historyAccumulator::[Mystate]}
instance Show Configuration where
 show (Configuration stm envV envP sto his) = unlines $ fx <$> his
  where
   fx (envV,envP,sto) = "\n\n\tVariable Environment \n" <> (show envV) <> "\n\n\tProcedure Environment\n" <> (show envP) <> "\n\tStore\n\n" <> (show sto)

--my lenses because lens library may be not present there
--lg--> lenseGetter4
--ls--> lenseSetter
--some getters for Configuration
lg_stm  = \(Configuration stm envV envP sto his) -> stm
lg_envV = \(Configuration stm envV envP sto his) -> envV
lg_envP = \(Configuration stm envV envP sto his) -> envP
lg_sto  = \(Configuration stm envV envP sto his) -> sto
lg_his  = \(Configuration stm envV envP sto his) -> his

--some setters for Configuration

ls_stm  stm = \(Configuration _ envV envP sto his) ->Configuration stm envV envP sto his
ls_envV envV= \(Configuration stm _ envP sto his)  ->Configuration stm envV envP sto his
ls_envP envP= \(Configuration stm envV _ sto his)  ->Configuration stm envV envP sto his
ls_sto  sto = \(Configuration stm envV envP _ his) ->Configuration stm envV envP sto his

lm_his mystate (Configuration stm envV envP sto his) = Configuration stm envV envP sto (his++[mystate])


data Config = Inter Stm Mystate | Final Mystate deriving Show

ns_stm::Config->Config
ns_stm = u

s_static::StateT Configuration Maybe Mystate
s_static = do
  stm <-gets lg_stm
  envV<-gets lg_envV
  envP<-gets lg_envP
  sto <-gets lg_sto
  let step =(envV,envP,sto)
  modify (lm_his step)
  conf <- get
  case stm of
   []                  -> return (envV,envP,sto)
   Skip:ns             -> do modify (ls_stm ns)
                             s_static
   (Ass var aexp):stms -> do updstore <-lift $liftM3 updateSto (getVarLoc var envV) (a_eval aexp envV sto) (pure sto)
                             modify (ls_sto updstore.ls_stm stms)
                             s_static
   (Comp ss1 ss2):stms -> do modify (ls_stm (ss1:ss2:stms))
                             s_static
   (If p ss1 ss2):stms -> do selectedbranch<-lift$liftM3 (\b g1 g2 ->if b then g1 else g2) (b_eval p envV sto) (pure ss1) (pure ss2)
                             modify (ls_stm (selectedbranch:stms))
                             s_static
   (While p ss):stms   -> do modify (ls_stm ((If p (Comp ss (While p ss)) Skip):stms))
                             s_static
   (Block decV decP ss):stms -> do (envV',sto') <- lift$ updateDv decV (envV,sto)
                                   envP' <- lift$updateDp decP envV' envP
                                   modify (ls_stm (ss:(AuxBlockEnvRes envV envP):stms).(ls_envP $ envP<>envP').(ls_envV $ envV<>envV').ls_sto sto')
                                   s_static
   (Call pname):stms         -> do (pBodystm,envV',envP') <- lift$ M.lookup pname (procMappings envP)
                                   modify (ls_stm (pBodystm:(AuxProcEnvRes envV envP):stms).(ls_envP (envP')).(ls_envV envV'))
                                   s_static
   (AuxBlockEnvRes envV' envP'):stms      -> do  modify (ls_stm stms.ls_envV (envV' `M.intersection` envV))
                                                 modify (ls_envP.EnvProc $ M.intersection (procMappings envP) (procMappings envP'))
                                                 s_static
   (AuxProcEnvRes envV' envP'):stms       -> do  modify (ls_stm stms.ls_envV envV.ls_envP envP)
                                                 s_static


--s_static,s_mixed,s_dynamic::Stm->EnvV->Store->Store


makeinitConf s=Configuration [s] mempty (EnvProc mempty)(Store mempty 1) []

s_dynamic=undefined
s_mixed=undefined

-- to check code run this with a test file

-- note var has to be decleared before use


compileTest filename=do
      h <-readFile filename
      let o = parse h
      putStrLn "PARSE TREE"
      print o
      let y =execStateT s_static (makeinitConf o)
      return y


-- Lexer definition

langDef = javaStyle {
  PT.reservedNames = ["skip", "while", "do", "var", "proc", "is", "call",
                      "if","then","else", "begin", "end", "true", "false"],
  PT.commentStart = "/*",
  PT.commentEnd = "*/",
  PT.commentLine = "//",
  PT.nestedComments = True
  }
lexer = PT.makeTokenParser langDef

identifier = PT.identifier lexer
integer = PT.integer lexer
symbol = PT.symbol lexer
parens  = PT.parens lexer
reserved = PT.reserved lexer
whiteSpace = PT.whiteSpace lexer

kw = reserved

type Parser a = ParsecT String () Identity a
-- Running parser

runParser :: String -> String -> Either ParseError Stm
runParser info input = Parsec.parse pProg info input

parse input = case runParser "input" input of
  Left e -> error (show e)
  Right s -> s
-- Parser for statement composition
-- The syntax is ambiguous as to priority and associativity of ";"
-- Assume a statement may be a list of single statements separated by ";"

pProg :: Parser Stm
pProg = do { whiteSpace; s <- pStm; eof; return s }

pStm :: Parser Stm
pStm = pStm1 `chainr1` pComp
pComp = symbol ";" >> return Comp

-- A single statement without semicolons on top level
pStm1 :: Parser Stm
pStm1 = pAss <|> pSkip <|> pIf <|> pWhile <|> pBlock <|> pCall <|> parens pStm

pSkip :: Parser Stm
pSkip = kw "skip" >> return Skip

pIf = do
  kw "if"
  b <- pBexp
  kw "then"
  s1 <- pStm
  kw "else"
  s2 <- pStm
  return $ If b s1 s2

-- Arithmetic expressions
pAexp, pTerm, pF :: Parser Aexp
pAexp = pTerm `chainl1` pAdd
pAdd = pBinOp "+" Add <|> pBinOp "-" Sub

pTerm = pF `chainl1` pBinOp "*" Mult
pF = (N <$> integer) <|> (V <$> identifier) <|> try (parens pAexp)

pBinOp :: String -> (a->b->c) -> Parser (a->b->c)
pBinOp sym con = symbol sym >> return con

-- Boolean expressions
pBexp, pTrue, pFalse, pCompare :: Parser Bexp
pBexp = pBexp1 `chainl1` pAnd
pAnd = symbol "&" >> return And
pBexp1 = Neg <$> (symbol "!" >> pBexp2) <|> pBexp2
pBexp2 = pTrue <|> pFalse <|> pCompare <|> try (parens pBexp)
pTrue = kw "true" >> return TRUE
pFalse = kw "false" >> return FALSE
pCompare = pAexp >>= pCompare'
pCompare' :: Aexp -> Parser Bexp
pCompare' a1 = go "=" Eq <|> go "<=" Le where
  go sym con = do {symbol sym; a2 <- pAexp; return (con a1 a2) }
-- pCompare' a1 = op <*> pure a1 <*> pAexp where
--   op = pBinOp "=" Eq <|> pBinOp "<=" Le

pBlock :: Parser Stm
pBlock = do
  kw "begin"
  vs <- pDecVs
  ps <- pDecPs
  stm <- pStm
  kw "end"
  return (Block vs ps stm)

pAss = do
  v <- identifier
  symbol ":="
  a <- pAexp
  return (Ass v a)

pDecVs :: Parser DecV
pDecVs = pDecV `endBy` (symbol ";")
pDecV :: Parser (Var,Aexp)
pDecV = do
  kw "var"
  v <- identifier
  symbol ":="
  a <- pAexp
  return (v,a)

pDecPs :: Parser DecP
pDecPs = pDecP `endBy` (symbol ";")
pDecP :: Parser (Pname,Stm)
pDecP = do
  kw "proc"
  n <- identifier
  kw "is"
  s <- pStm1
  return (n,s)

pWhile :: Parser Stm
pWhile = do
  kw "while"
  b <- pBexp
  kw "do"
  s <- pStm
  return (While b s)

pCall :: Parser Stm
pCall = kw "call" >> (Call <$> identifier)

main = do
  print $ parse test1
  print $ parse test2

t1= parse test1
t2= parse test2
test1 = "/*fac_loop (p.23)*/\ny:=1;\nwhile !(x=1) do (\n y:=y*x;\n x:=x-1\n)"
test2 = "//fac call (p.55)\n\
\begin\n\
\proc fac is\n\
\begin\n\
\var z:=x;\n\
\if x=1 then skip\n\
\else x:=x-1; call fac; y:=z*y end;\n\
\y:=1; call fac end"
