{-# LANGUAGE StandaloneDeriving #-}
module ParseWhile where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List (intercalate)

type Num = Integer
type Var = String

type State = Var -> Z

type Z = Integer
type T = Bool
type Pname = String


type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type Loc = Num

type EnvV = Var -> Loc
type EnvP = Pname -> Stm

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

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> pure ()
