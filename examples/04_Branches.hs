module Example where

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, hellos, sign] []

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , ldc_i4 4
  , call [] Void "" "Haskell.Ehc.Hello" "hellos" [Int32]

  , call [] Void "mscorlib" "System.Console" "WriteLine" []

  , ldc_i4 (-4)
  , call [] Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 0
  , call [] Void "" "Haskell.Ehc.Hello" "sign" [Int32]
  , ldc_i4 4
  , call [] Void "" "Haskell.Ehc.Hello" "sign" [Int32]

  , ret
  ]

hellos :: MethodDef
hellos = Method [MaStatic, MaPublic] Void "hellos" [Param Nothing Int32 "x"]
  [ maxStack 4
  , localsInit
      [ Local Int32 "i"
      ]

  , ldarg 0
  , stloc 0
  , br "Test"
  , label "Loop"
  $ ldstr "Hello!"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
  , ldloc 0
  , ldc_i4 1
  , sub
  , stloc 0
  , label "Test"
  $ ldloc 0
  , ldc_i4 0
  , bgt "Loop"
  , ret
  ]

sign :: MethodDef
sign = Method [MaStatic, MaPublic] Void "sign" [Param Nothing Int32 "x"]
  [ maxStack 4

  , ldarg 0
  , ldc_i4 0
  , bge "TestPositive"
  , ldstr "input is negative!"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "TestPositive"
  $ ldarg 0
  , brtrue "Positive"
  , ldstr "input is zero!"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
  , br "End"

  , label "Positive"
  $ ldstr "input is positive!"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
  , label "End"
  $ ret
  ]

