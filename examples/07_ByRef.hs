module Example where

import Language.Cil

main = putStr (pr ass "")

ass :: Assembly
ass = Assembly [mscorlibRef] "Example" [hello]

hello :: TypeDef
hello = classDef [CaPublic] "Haskell.Ehc.Hello" noExtends noImplements
                 [] [myMain, updateRef] [myClass]

myMain :: MethodDef
myMain = Method [MaStatic, MaPublic] Void "main" []
  [ localsInit
      [ Local (ReferenceType "" "Haskell.Ehc.Hello/MyClass") "c" ]
  , entryPoint

  , ldc_i4 2
  , newobj "" "Haskell.Ehc.Hello/MyClass" [Int32]
  , stlocN "c"

  , ldloca 0
  , call [] Void "" "Haskell.Ehc.Hello" "updateRef" [ByRef $ ReferenceType "" "Haskell.Ehc.Hello/MyClass"]

  , ldlocN "c"
  , ldflda Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , call [CcInstance] String "" "int32" "ToString" []
  , call [] Void "mscorlib" "System.Console" "WriteLine" [String]

  , ret
  ]

updateRef :: MethodDef
updateRef = Method [MaStatic, MaPublic] Void "updateRef" [Param Nothing (ByRef $ ReferenceType "" "Haskell.Ehc.Hello/MyClass") "c"]
  [ ldarg 0
  , ldc_i4 3
  , newobj "" "Haskell.Ehc.Hello/MyClass" [Int32]
  , stind_ref
  , ret
  ]

myClass :: TypeDef
myClass = classDef [CaNestedPrivate] "MyClass" noExtends noImplements
                   [value] [ctor, toString] []

value :: FieldDef
value = Field [FaPublic] Int32 "Value"

ctor :: MethodDef
ctor = Constructor [MaPublic] Void [Param Nothing Int32 "value"]
  [ ldarg 0
  , call [CcInstance] Void "" "object" ".ctor" []
  , ldarg 0
  , ldarg 1
  , stfld Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , ret
  ]

toString :: MethodDef
toString = Method [MaPublic] String "ToString" []
  [ ldarg 0
  , ldflda Int32 "" "Haskell.Ehc.Hello/MyClass" "Value"
  , call [CcInstance] String "" "int32" "ToString" []
  , ret
  ]

