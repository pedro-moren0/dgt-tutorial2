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
tipoOp AddInt = FunctionT [IntT, IntT] IntT
tipoOp SubInt = FunctionT [IntT, IntT] IntT
tipoOp MultInt = FunctionT [IntT, IntT] IntT
tipoOp DivInt = FunctionT [IntT, IntT] IntT
tipoOp ModInt = FunctionT [IntT, IntT] IntT
tipoOp LTEInt = FunctionT [IntT, IntT] BoolT
tipoOp AddFloat = FunctionT [FloatT, FloatT] FloatT
tipoOp SubFloat = FunctionT [FloatT, FloatT] FloatT
tipoOp MultFloat = FunctionT [FloatT, FloatT] FloatT
tipoOp DivFloat = FunctionT [FloatT, FloatT] FloatT
tipoOp Sqrt = FunctionT [FloatT] FloatT
tipoOp EqChar = FunctionT [CharT, CharT] BoolT
tipoOp EqInt = FunctionT [IntT, IntT] BoolT
tipoOp IsLetter = FunctionT [CharT] BoolT
tipoOp IsDigit = FunctionT [CharT] BoolT
tipoOp ToUpper = FunctionT [CharT] CharT
tipoOp ToLower = FunctionT [CharT] CharT
tipoOp And = FunctionT [BoolT, BoolT] BoolT
tipoOp Or = FunctionT [BoolT, BoolT] BoolT
tipoOp Not = FunctionT [BoolT] BoolT
tipoOp If = FunctionT [BoolT, PolyT 0, PolyT 0] (PolyT 0)
tipoOp ToPair = FunctionT [PolyT 0, PolyT 1] (PairT (PolyT 0) (PolyT 1))
tipoOp Fst = FunctionT [PairT (PolyT 0) (PolyT 1)] (PolyT 0)
tipoOp Snd = FunctionT [PairT (PolyT 0) (PolyT 1)] (PolyT 1)
tipoOp Len = FunctionT [ListT $ PolyT 0] IntT
tipoOp Cons = FunctionT [PolyT 0, ListT (PolyT 0)] (PolyT 0)
tipoOp Head = FunctionT [ListT (PolyT 0)] (PolyT 0)
tipoOp Tail = FunctionT [ListT $ PolyT 0] (ListT $ PolyT 0)
tipoOp Map = FunctionT [LambdaT $ FunctionT [PolyT 0] (PolyT 1), ListT $ PolyT 0] (PolyT 1)
tipoOp Filter = FunctionT [LambdaT $ FunctionT [PolyT 0] BoolT, ListT $ PolyT 0] (PolyT 0)

ariOp :: Op -> Int
ariOp = length . _argT . tipoOp
