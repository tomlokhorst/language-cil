module Example where

import Prelude hiding (rem)

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, myAdd, myEven] []

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , ldc_i4 3
  , ldc_i4 2
  , call [] Int32 "" "Haskell.Ehc.Hello" "add" [Int32, Int32]
  , call [] Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ldc_i4 3
  , call [] Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call [] Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myAdd :: MethodDef
myAdd = Method [MaStatic, MaPublic] Int32 "add" [Param Nothing Int32 "x", Param Nothing Int32 "y"]
  [ maxStack 2
  , ldargN "x"
  , ldargN "y"
  , add
  , ret
  ]

myEven :: MethodDef
myEven = Method [MaStatic, MaPublic] Bool "even" [Param Nothing Int32 "x"]
  [ localsInit
      [ Local Int32 "r"
      , Local Bool "b"
      ]
  , ldarg 0
  , ldc_i4 2
  , rem
  , stloc 0
  , ldloc 0
  , ldc_i4 0
  , ceq
  , stloc 1
  , ldloc 1
  , ret
  ]

