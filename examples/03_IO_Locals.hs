module Example where

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, ioAge] []

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , call [] Void "" "Haskell.Ehc.Hello" "ioAge" []

  , ret
  ]

ioAge :: MethodDef
ioAge = Method [MaStatic, MaPublic] Void "ioAge" []
  [ maxStack 11
  , localsInit
      [ Local Int32 "x"
      , Local (ValueType "mscorlib" "System.DateTime") "d1"
      , Local (ValueType "mscorlib" "System.DateTime") "d2"
      ]

  , ldstr "What year were you born?"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
  , call [] String "mscorlib" "System.Console" "ReadLine" []
  , call [] Int32 "" "int32" "Parse" [String]
  , stloc 0
  , call [] (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "get_Now" []
  , stloc 1
  , ldloca 1
  , ldloc 0
  , neg
  , call [CcInstance] (ValueType "mscorlib" "System.DateTime") "mscorlib" "System.DateTime" "AddYears" [Int32]
  , stloc 2
  , ldstr "This year, you turn {0}."
  , ldloca 2
  , call [CcInstance] Int32 "mscorlib" "System.DateTime" "get_Year" []
  , box Int32
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String, Object]
  , ret
  ]

