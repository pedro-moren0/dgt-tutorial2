module Grammar.Utils (module Grammar.Utils) where

import Grammar.Core

tipoLit :: Lit -> Tipo
tipoLit (IntL _) = IntT
tipoLit (FloatL _) = FloatT
tipoLit (CharL _) = CharT
tipoLit (BoolL _) = BoolT
tipoLit (PairL x y) = PairT (tipoLit x) (tipoLit y)
tipoLit (ListL t _) = t
tipoLit (LambdaL (TAST _ ftype)) = LambdaT ftype

tipoOp :: Op -> FunctionT
tipoOp AddInt = [IntT, IntT] --> IntT
tipoOp SubInt = [IntT, IntT] --> IntT
tipoOp MultInt = [IntT, IntT] --> IntT
tipoOp DivInt = [IntT, IntT] --> IntT
tipoOp ModInt = [IntT, IntT] --> IntT
tipoOp LTEInt = [IntT, IntT] --> BoolT
tipoOp AddFloat = [FloatT, FloatT] --> FloatT
tipoOp SubFloat = [FloatT, FloatT] --> FloatT
tipoOp MultFloat = [FloatT, FloatT] --> FloatT
tipoOp DivFloat = [FloatT, FloatT] --> FloatT
tipoOp Sqrt = [FloatT] --> FloatT
tipoOp EqChar = [CharT, CharT] --> BoolT
tipoOp EqInt = [IntT, IntT] --> BoolT
tipoOp IsLetter = [CharT] --> BoolT
tipoOp IsDigit = [CharT] --> BoolT
tipoOp ToUpper = [CharT] --> CharT
tipoOp ToLower = [CharT] --> CharT
tipoOp And = [BoolT, BoolT] --> BoolT
tipoOp Or = [BoolT, BoolT] --> BoolT
tipoOp Not = [BoolT] --> BoolT
tipoOp If = [BoolT, PolyT 0, PolyT 0] --> PolyT 0
tipoOp ToPair = [PolyT 0, PolyT 1] --> PairT (PolyT 0) (PolyT 1)
tipoOp Fst = [PairT (PolyT 0) (PolyT 1)] --> PolyT 0
tipoOp Snd = [PairT (PolyT 0) (PolyT 1)] --> PolyT 1
tipoOp Len = [ListT $ PolyT 0] --> IntT
tipoOp Cons = [PolyT 0, ListT (PolyT 0)] --> PolyT 0
tipoOp Head = [ListT (PolyT 0)] --> PolyT 0
tipoOp Tail = [ListT $ PolyT 0] --> ListT (PolyT 0)
tipoOp Map = [LambdaT $ [PolyT 0] --> PolyT 1, ListT $ PolyT 0] --> PolyT 1
tipoOp Filter = [LambdaT $ [PolyT 0] --> BoolT, ListT $ PolyT 0] --> PolyT 0

ariOp :: Op -> Int
ariOp = length . _argT . tipoOp
