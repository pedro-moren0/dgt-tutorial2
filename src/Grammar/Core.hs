module Grammar.Core (module Grammar.Core) where

-- | ########### TIPOS ###########
data Tipo
  = IntT
  | FloatT
  | CharT
  | BoolT
  | PolyT Int
  | PairT Tipo Tipo
  | ListT Tipo
  | LambdaT FunctionT
  deriving (Show, Eq)

data FunctionT = FunctionT {_argT :: ArgT, _outT :: OutT} deriving (Show, Eq)

type ArgT = [Tipo]

type OutT = Tipo

-- | ########### AST ###########
data Lit
  = IntL Int
  | FloatL Float
  | CharL Char
  | BoolL Bool
  | PairL Lit Lit
  | ListL Tipo [Lit]
  | LambdaL TAST
  deriving (Show)

data Op
  = AddInt
  | SubInt
  | MultInt
  | DivInt
  | ModInt
  | LTEInt
  | EqInt
  | AddFloat
  | SubFloat
  | MultFloat
  | DivFloat
  | Sqrt
  | EqChar
  | IsLetter
  | IsDigit
  | ToUpper
  | ToLower
  | And
  | Or
  | Not
  | If
  | ToPair
  | Fst
  | Snd
  | Len
  | Cons
  | Head
  | Tail
  | Map
  | Filter
  deriving (Show)

data Terminal = Literal Lit | Arg Int deriving (Show)

data AST = Leaf Terminal | Node Op [AST] deriving (Show)

data TAST = TAST {_ast :: AST, _ftype :: FunctionT} deriving (Show)

-- | ########### UTILIDADES ###########
(-<) :: Op -> [AST] -> AST
(-<) = Node

(-->) :: ArgT -> OutT -> FunctionT
(-->) = FunctionT

(\.) :: AST -> FunctionT -> AST
(\.) ast ftype = Leaf $ Literal $ LambdaL $ TAST ast ftype