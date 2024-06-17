{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Examples (module Examples) where

import GHC.TypeNats (KnownNat (natSing))
import Grammar.Core

-- | Uma expressao
newExpr1 :: AST '[] Int
newExpr1 =
  If
    (IsDigit (C $ CharL 'c'))
    ( SubInt
        ( AddInt
            (C (IntL 2))
            ( MultInt
                (C (IntL 5))
                (C (IntL 3))
            )
        )
        (C (IntL 7))
    )
    (C (IntL (-1)))

zd :: AST '[] Int
zd = DivInt (C (IntL 2)) (C (IntL 0))

-- | Calcula a minha idade em um dado ano
-- |
-- | Arg 0: Ano em que quero saber minha idade
-- | Arg 1: Ano em que eu nasci
-- | Retorna: minha idade no ano fornecido
ageIn :: AST '[Int, Int] Int
ageIn = SubInt (A $ natSing @0) (A $ natSing @1)

-- | Calcula a distancia euclidiana de dois pontos no plano
-- |
-- | Arg 0: Primeiro ponto
-- | Arg 1: Segundo ponto
-- | Retorna: a distancia entre os dois pontos fornecidos
dist :: AST '[(Float, Float), (Float, Float)] Float
dist =
  Sqrt
    ( AddFloat
        ( MultFloat
            ( SubFloat
                (Fst (A (natSing @0)))
                (Fst (A (natSing @1)))
            )
            ( SubFloat
                (Fst (A (natSing @0)))
                (Fst (A (natSing @1)))
            )
        )
        ( MultFloat
            ( SubFloat
                (Snd (A (natSing @0)))
                (Snd (A (natSing @1)))
            )
            ( SubFloat
                (Snd (A (natSing @0)))
                (Snd (A (natSing @1)))
            )
        )
    )

-- | Eleva os elementos de uma lista de inteiros ao quadrado
-- | e filtra os elementos pares
-- |
-- | Arg 0: uma lista de inteiros
-- | Retorna: uma lista com todos os quadrados pares
squareAndFilterEvens :: AST '[[Int]] [Int]
squareAndFilterEvens =
  Filter
    ( Equals
        ( ModInt
            (A (natSing @0))
            (C (IntL 2))
        )
        (C (IntL 0))
    )
    ( Map
        ( MultInt
            (A (natSing @0))
            (A (natSing @0))
        )
        (A (natSing @0))
    )