module Example where

import Prelude hiding (tail)

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain] []

int16 = ValueType "mscorlib" "System.Int16"

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint
  , localsInit [Local Int32 "x", Local Int32 "i"]

  , ldc_i4 5 -- we will sum the first 5 primes
  , newarr Int32

  , dup
  , ldc_i4 0
  , ldc_i4 2
  , stelem_i4

  , dup
  , ldc_i4 1
  , ldc_i4 3
  , stelem_i4

  , dup
  , ldc_i4 2
  , ldc_i4 5
  , stelem_i4

  , dup
  , ldc_i4 3
  , ldc_i4 7
  , stelem_i4

  , dup
  , ldc_i4 4
  , ldc_i4 11
  , stelem_i4

  , ldc_i4 0
  , stlocN "i"

  , ldc_i4 0
  , stlocN "x"

  , label "loop"
  $ dup
  , dup
  , ldlocN "i"
  , ldelem_i4
  , ldlocN "x"
  , add
  , stlocN "x"
  , ldlocN "i"
  , ldc_i4 1
  , add
  , stlocN "i"
  , ldlen
  , ldlocN "i"
  , bgt "loop"

  , pop
  , ldlocN "x"

  , call [] Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ret
  ]
