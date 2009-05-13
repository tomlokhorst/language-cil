module Example where

import Language.Cil

main = putStr (cil ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, doNothing] []

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , ldstr "Hello, World!"
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]

  , call [] Void "" "Haskell.Ehc.Hello" "doNothing" []

  , ret
  ]

doNothing :: MethodDef
doNothing = Method [MaStatic, MaPublic] Void "doNothing" []
  [ ret ]

