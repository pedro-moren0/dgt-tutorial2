{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Examples (module Examples) where

import Data.Void
import GHC.TypeNats (KnownNat (natSing), SNat)
import Grammar
import HList

-- | Uma expressao
expr1 :: AST
expr1 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (CharL 'c')],
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

x :: AST' Int '[Int, Char, Int]
x = A $ natSing @2

e1 :: AST' Int '[Int, Char, Char]
e1 = AddInt' (C $ IntL' 1) (C $ IntL' 1)

-- e2 :: AST' Bool '[Int, Char, Char]
-- e2 = GTEInt' e1 (C $ IntL' 3)

-- e3 :: AST' Int '[Lit' Int, Lit' Char]
-- e3 =
--   If'
--     (IsDigit' $ A 0)
--     ( SubInt'
--         ( AddInt'
--             (C $ IntL' 2)
--             (MultInt' (C $ IntL' 5) (C $ IntL' 3))
--         )
--         (C $ IntL' 7)
--     )
--     (C $ IntL' (-1))

-- | Outra expressao
expr2 :: AST
expr2 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (CharL '1')],
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

-- | Uma expressao com erro
exprErr1 :: AST
exprErr1 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (IntL 1)],
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

-- | Outra expressao com erro
exprErr2 :: AST
exprErr2 =
  If
    -< [ IsDigit
           -< [Leaf $ Literal (CharL '1')],
         SubInt
           -< [ AddInt
                  -< [ Leaf $ Literal (IntL 2),
                       MultInt
                         -< [ Leaf $ Literal (IntL 5),
                              Leaf $ Literal (IntL 3)
                            ]
                     ],
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7),
                Leaf $ Literal (IntL 7)
              ],
         ToUpper
           -< [Leaf $ Literal (CharL 'c')]
       ]

-- | Calcula a minha idade em um dado ano
-- |
-- | Arg 0: Ano em que quero saber minha idade
-- | Arg 1: Ano em que eu nasci
-- | Retorna: minha idade no ano fornecido
ageIn :: AST
ageIn =
  SubInt
    -< [ Leaf $ Arg 0,
         Leaf $ Arg 1
       ]

-- ageIn' :: AST' Int
-- ageIn' = Node' (AddInt' (Leaf' $ Arg' 0) (Leaf' $ Arg' 1))

-- | Calcula a distancia euclidiana de dois pontos no plano
-- |
-- | Arg 0: Primeiro ponto
-- | Arg 1: Segundo ponto
-- | Retorna: a distancia entre os dois pontos fornecidos
dist :: AST
dist =
  Sqrt
    -< [ AddFloat
           -< [ MultFloat
                  -< [ SubFloat
                         -< [ Fst -< [Leaf $ Arg 1],
                              Fst -< [Leaf $ Arg 0]
                            ],
                       SubFloat
                         -< [ Fst -< [Leaf $ Arg 1],
                              Fst -< [Leaf $ Arg 0]
                            ]
                     ],
                MultFloat
                  -< [ SubFloat
                         -< [ Snd -< [Leaf $ Arg 1],
                              Snd -< [Leaf $ Arg 0]
                            ],
                       SubFloat
                         -< [ Snd -< [Leaf $ Arg 1],
                              Snd -< [Leaf $ Arg 0]
                            ]
                     ]
              ]
       ]

-- | Soma +1 a todos os elementos de uma lista de inteiros
-- |
-- | Arg 0: A lista de elementos inteiros a ser mapeada
-- | Retorna: a lista de elementos fornecida +1 elemento a elemento
plus1 :: AST
plus1 =
  Map
    -< [ (\.) (AddInt -< [Leaf $ Arg 0, Leaf $ Literal (IntL 1)]) ([IntT] --> IntT),
         Leaf $ Literal $ ListL IntT [IntL 1, IntL 2, IntL 3]
       ]

-- | Filtra os elementos <= 3 de uma lista de inteiros
-- |
-- | Arg 0: a lista a ser filtrada
-- | Retorna: a lista com todas as ocorrencias <= 3
lte3 :: AST
lte3 =
  Filter
    -< [ (\.)
           (LTEInt -< [Leaf $ Arg 0, Leaf $ Literal $ IntL 3])
           ([IntT] --> BoolT),
         Leaf $ Arg 0
       ]

-- | Eleva os elementos de uma lista de inteiros ao quadrado
-- | e filtra os elementos pares
-- |
-- | Arg 0: uma lista de inteiros
-- | Retorna: uma lista com todos os quadrados pares
squareAndFilterEvens :: AST
squareAndFilterEvens =
  Filter
    -< [ (\.)
           ( EqInt
               -< [ ModInt -< [Leaf $ Arg 0, Leaf $ Literal (IntL 2)],
                    Leaf $ Literal (IntL 0)
                  ]
           )
           ([IntT] --> BoolT),
         Map
           -< [ (\.)
                  (MultInt -< [Leaf $ Arg 0, Leaf $ Arg 0])
                  ([IntT] --> IntT),
                Leaf $ Arg 0
              ]
       ]