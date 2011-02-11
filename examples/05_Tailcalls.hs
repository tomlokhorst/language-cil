module Example where

import Prelude hiding (tail)

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, myEven, myOdd] []

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , ldc_i4 1000000
  , call [] Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , call [] Void "mscorlib" "System.Console" "WriteLine" [Bool]

  , ret
  ]

myEven :: MethodDef
myEven = Method [MaStatic, MaPublic] Bool "even" [Param Nothing Int32 "x"]
  [ ldarg 0
  , brtrue "else"
  , ldc_i4 1
  , ret
  , label "else"
  , ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call [] Bool "" "Haskell.Ehc.Hello" "odd" [Int32]
  , ret
  ]

myOdd :: MethodDef
myOdd = Method [MaStatic, MaPublic] Bool "odd" [Param Nothing Int32 "x"]
  [ ldarg 0
  , brtrue "else"
  , ldc_i4 0
  , ret
  , label "else"
  , ldarg 0
  , ldc_i4 1
  , sub
  , tailcall
  $ call [] Bool "" "Haskell.Ehc.Hello" "even" [Int32]
  , ret
  ]

