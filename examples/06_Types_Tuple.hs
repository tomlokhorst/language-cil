module Example where

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, pair] [tuple2]

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ entryPoint

  , ldc_i4 3
  , ldc_i4 2
  , newobj "" "Haskell.Ehc.Hello/Tuple" [Int32, Int32]
  , call [] Int32 "" "Haskell.Ehc.Hello" "pairadd" [ReferenceType "" "Haskell.Ehc.Hello/Tuple"]

  , call [] Void "mscorlib" "System.Console" "WriteLine" [Int32]

  , ret
  ]

pair :: MethodDef
pair = Method [MaStatic, MaPublic] Int32 "pairadd" [Param Nothing (ReferenceType "" "Haskell.Ehc.Hello/Tuple") "t"]
  [ ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Fst"
  , ldarg 0
  , ldfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Snd"
  , add
  , ret
  ]

tuple2 :: TypeDef
tuple2 = classDef [CaNestedPrivate] "Tuple" noExtends noImplements
                  [myFst, mySnd] [tupleCtor] []

myFst :: FieldDef
myFst = Field [FaStatic, FaPublic] Int32 "Fst"

mySnd :: FieldDef
mySnd = Field [FaStatic, FaPublic] Int32 "Snd"

tupleCtor :: MethodDef
tupleCtor = Constructor [MaPublic] Void [Param Nothing Int32 "fst", Param Nothing Int32 "snd"]
  [ ldarg 0
  , call [CcInstance] Void "" "object" ".ctor" []
  , ldarg 0
  , ldarg 1
  , stfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Fst"
  , ldarg 0
  , ldarg 2
  , stfld Int32 "" "Haskell.Ehc.Hello/Tuple" "Snd"
  , ret
  ]

