module Example where

import Prelude hiding (tail)

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain] []

deadbeef = 3735928559

int16 = ValueType "mscorlib" "System.Int16"

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint
  , localsInit [Local Int32 "x"]

  , ldc_i4 deadbeef
  , stloc 0

  , ldstr "First {0:X} then {1:X} then {2:X}"

  , ldloca 0
  , ldind_i2
  , box int16

  , ldloca 0
  , ldc_i4 1
  , add
  , unaligned ByteAligned ldind_i2
  , box int16

  , ldloca 0
  , ldc_i4 2
  , add
  , unaligned DoubleByteAligned ldind_i2
  , box int16

  , call [] Void "mscorlib" "System.Console" "WriteLine" [String, Object, Object, Object]

  , ret
  ]