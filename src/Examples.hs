{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Examples (module Examples) where

import GHC.TypeNats (KnownNat (natSing))
import Grammar.Core

-- | Uma expressao
--   If
--     -< [ IsDigit
--            -< [Leaf $ Literal (CharL 'c')],
--          SubInt
--            -< [ AddInt
--                   -< [ Leaf $ Literal (IntL 2),
--                        MultInt
--                          -< [ Leaf $ Literal (IntL 5),
--                               Leaf $ Literal (IntL 3)
--                             ]
--                      ],
--                 Leaf $ Literal (IntL 7)
--               ],
--          ToUpper
--            -< [Leaf $ Literal (CharL 'c')]
--        ]
x :: AST '[Int, Char, Int] Int
x = A $ natSing @2

e1 :: AST '[Int, Char, Char] Int
e1 = AddInt (C $ IntL 1) (C $ IntL 1)

square :: AST '[] [Int]
square =
  Map
    ( MultInt (A $ natSing @0) (A $ natSing @0)
    )
    (C $ ListL $ IntL <$> [1, 2, 3, 4, 5])

evens :: AST '[Int, Int, Int] [Int]
evens =
  Filter
    (Equals (ModInt (A $ natSing @0) (C $ IntL 2)) (C $ IntL 0))
    squareInput

odds :: AST '[Int, Int, Int] [Int]
odds =
  Filter
    (Equals (ModInt (A $ natSing @0) (C $ IntL 2)) (C $ IntL 1))
    squareInput

squareInput :: AST '[Int, Int, Int] [Int]
squareInput =
  Map
    ( MultInt (A $ natSing @0) (A $ natSing @0)
    )
    (Cons (A $ natSing @2) (Cons (A $ natSing @1) (Cons (A $ natSing @0) (C $ ListL []))))

-- | Outra expressao
-- expr2 :: AST
-- expr2 =
--   If
--     -< [ IsDigit
--            -< [Leaf $ Literal (CharL '1')],
--          SubInt
--            -< [ AddInt
--                   -< [ Leaf $ Literal (IntL 2),
--                        MultInt
--                          -< [ Leaf $ Literal (IntL 5),
--                               Leaf $ Literal (IntL 3)
--                             ]
--                      ],
--                 Leaf $ Literal (IntL 7)
--               ],
--          ToUpper
--            -< [Leaf $ Literal (CharL 'c')]
--        ]

-- | Uma expressao com erro
-- exprErr1 :: AST
-- exprErr1 =
--   If
--     -< [ IsDigit
--            -< [Leaf $ Literal (IntL 1)],
--          SubInt
--            -< [ AddInt
--                   -< [ Leaf $ Literal (IntL 2),
--                        MultInt
--                          -< [ Leaf $ Literal (IntL 5),
--                               Leaf $ Literal (IntL 3)
--                             ]
--                      ],
--                 Leaf $ Literal (IntL 7)
--               ],
--          ToUpper
--            -< [Leaf $ Literal (CharL 'c')]
--        ]

-- | Outra expressao com erro
-- exprErr2 :: AST
-- exprErr2 =
--   If
--     -< [ IsDigit
--            -< [Leaf $ Literal (CharL '1')],
--          SubInt
--            -< [ AddInt
--                   -< [ Leaf $ Literal (IntL 2),
--                        MultInt
--                          -< [ Leaf $ Literal (IntL 5),
--                               Leaf $ Literal (IntL 3)
--                             ]
--                      ],
--                 Leaf $ Literal (IntL 7),
--                 Leaf $ Literal (IntL 7),
--                 Leaf $ Literal (IntL 7),
--                 Leaf $ Literal (IntL 7)
--               ],
--          ToUpper
--            -< [Leaf $ Literal (CharL 'c')]
--        ]

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
-- dist :: AST
-- dist =
--   Sqrt
--     -< [ AddFloat
--            -< [ MultFloat
--                   -< [ SubFloat
--                          -< [ Fst -< [Leaf $ Arg 1],
--                               Fst -< [Leaf $ Arg 0]
--                             ],
--                        SubFloat
--                          -< [ Fst -< [Leaf $ Arg 1],
--                               Fst -< [Leaf $ Arg 0]
--                             ]
--                      ],
--                 MultFloat
--                   -< [ SubFloat
--                          -< [ Snd -< [Leaf $ Arg 1],
--                               Snd -< [Leaf $ Arg 0]
--                             ],
--                        SubFloat
--                          -< [ Snd -< [Leaf $ Arg 1],
--                               Snd -< [Leaf $ Arg 0]
--                             ]
--                      ]
--               ]
--        ]

-- | Soma +1 a todos os elementos de uma lista de inteiros
-- |
-- | Arg 0: A lista de elementos inteiros a ser mapeada
-- | Retorna: a lista de elementos fornecida +1 elemento a elemento
-- plus1 :: AST
-- plus1 =
--   Map
--     -< [ (\.) (AddInt -< [Leaf $ Arg 0, Leaf $ Literal (IntL 1)]) ([IntT] --> IntT),
--          Leaf $ Literal $ ListL IntT [IntL 1, IntL 2, IntL 3]
--        ]

-- | Filtra os elementos <= 3 de uma lista de inteiros
-- |
-- | Arg 0: a lista a ser filtrada
-- | Retorna: a lista com todas as ocorrencias <= 3
-- lte3 :: AST
-- lte3 =
--   Filter
--     -< [ (\.)
--            (LTEInt -< [Leaf $ Arg 0, Leaf $ Literal $ IntL 3])
--            ([IntT] --> BoolT),
--          Leaf $ Arg 0
--        ]

-- | Eleva os elementos de uma lista de inteiros ao quadrado
-- | e filtra os elementos pares
-- |
-- | Arg 0: uma lista de inteiros
-- | Retorna: uma lista com todos os quadrados pares
-- squareAndFilterEvens :: AST
-- squareAndFilterEvens =
--   Filter
--     -< [ (\.)
--            ( EqInt
--                -< [ ModInt -< [Leaf $ Arg 0, Leaf $ Literal (IntL 2)],
--                     Leaf $ Literal (IntL 0)
--                   ]
--            )
--            ([IntT] --> BoolT),
--          Map
--            -< [ (\.)
--                   (MultInt -< [Leaf $ Arg 0, Leaf $ Arg 0])
--                   ([IntT] --> IntT),
--                 Leaf $ Arg 0
--               ]
--        ]