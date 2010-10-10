-- |
-- Combinators for building abstract syntax.
--

module Language.Cil.Build (

  -- * Assembly ref functions
    assemblyRef

  -- * Directive functions
  , entryPoint
  , localsInit
  , maxStack

  -- * mdecl functions
  , add
  , beq
  , bge
  , bgt
  , ble
  , blt
  , box
  , br
  , brfalse
  , brtrue
  , call
  , callvirt
  , ceq
  , cge
  , cgt
  , cle
  , clt 
  , dup
  , isinst
  , ldarg
  , ldargN
  , ldc_i4
  , ldchar
  , ldfld
  , ldflda
  , ldftn
  , ldind_i
  , ldind_i1
  , ldind_i2
  , ldind_i4
  , ldind_i8
  , ldind_r4
  , ldind_r8
  , ldind_ref
  , ldind_u1
  , ldind_u2
  , ldind_u4
  , ldloc
  , ldlocN
  , ldloca
  , ldlocaN
  , ldsfld
  , ldsflda
  , ldstr
  , mul
  , neg
  , newobj
  , nop
  , pop
  , rem
  , ret
  , stfld
  , stind_i
  , stind_i1
  , stind_i2
  , stind_i4
  , stind_i8
  , stind_r4
  , stind_r8
  , stind_ref
  , stloc
  , stlocN
  , stsfld
  , sub
  , tail
  , tailcall
  , unbox

  -- * Convenient AST functions
  , label
  , comment
  , extends
  , noExtends
  , noImplements
  , classDef
  , defaultCtor
  , extendsCtor
  , simpleAssembly
  , mscorlibRef
  ) where

-- If someone uses the `rem' or `tail' opcode, they can deal with the ambiguous
-- occurence themselves!
import Prelude hiding (rem, tail)
import Data.Char (ord)

import Language.Cil.Syntax

-- AssemblyRef functions

assemblyRef :: AssemblyName -> Version -> PublicKeyToken -> AssemblyRef
assemblyRef = AssemblyRef

-- Directive functions

entryPoint :: MethodDecl
entryPoint = Directive EntryPoint 

localsInit :: [Local] -> MethodDecl
localsInit ls = Directive (LocalsInit ls)

maxStack :: Int -> MethodDecl
maxStack x = Directive (MaxStack x)


-- mdecl functions

add :: MethodDecl
add = mdecl $ Add

beq :: Label -> MethodDecl
beq = mdecl . Beq

bge :: Label -> MethodDecl
bge = mdecl . Bge

bgt :: Label -> MethodDecl
bgt = mdecl . Bgt

ble :: Label -> MethodDecl
ble = mdecl . Ble

blt :: Label -> MethodDecl
blt = mdecl . Blt

box :: PrimitiveType -> MethodDecl
box = mdecl . Box

unbox :: PrimitiveType -> MethodDecl
unbox = mdecl . Unbox

br :: Label -> MethodDecl
br = mdecl . Br

brfalse :: Label -> MethodDecl
brfalse = mdecl . Brfalse

brtrue :: Label -> MethodDecl
brtrue = mdecl . Brtrue

call :: [CallConv] -> PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
call ccs p l t m ps = mdecl $ Call ccs p l t m ps

callvirt :: PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
callvirt p l t m ps = mdecl $ CallVirt p l t m ps

ceq, cge, cgt, cle, clt :: MethodDecl
ceq = mdecl $ Ceq
cge = mdecl $ Cge
cgt = mdecl $ Cgt
cle = mdecl $ Cle
clt = mdecl $ Clt

dup :: MethodDecl
dup = mdecl $ Dup

isinst :: TypeName -> MethodDecl
isinst = mdecl . Isinst

ldarg :: Offset -> MethodDecl
ldarg 0 = mdecl $ Ldarg_0
ldarg 1 = mdecl $ Ldarg_1
ldarg 2 = mdecl $ Ldarg_2
ldarg 3 = mdecl $ Ldarg_3
ldarg x = mdecl $ Ldarg x

ldargN :: DottedName -> MethodDecl
ldargN = mdecl . LdargN

ldc_i4 :: Int -> MethodDecl
ldc_i4 (-1) = mdecl $ Ldc_i4_m1
ldc_i4 0    = mdecl $ Ldc_i4_0
ldc_i4 1    = mdecl $ Ldc_i4_1
ldc_i4 2    = mdecl $ Ldc_i4_2
ldc_i4 3    = mdecl $ Ldc_i4_3
ldc_i4 4    = mdecl $ Ldc_i4_4
ldc_i4 5    = mdecl $ Ldc_i4_5
ldc_i4 6    = mdecl $ Ldc_i4_6
ldc_i4 7    = mdecl $ Ldc_i4_7
ldc_i4 8    = mdecl $ Ldc_i4_8
ldc_i4 x    = mdecl $ if -127 <= x && x <= 128
                      then Ldc_i4_s x
                      else Ldc_i4 x

ldchar :: Char -> MethodDecl
ldchar c = ldc_i4 (ord c)

ldfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldfld p a t f = mdecl $ Ldfld p a t f

ldflda :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldflda p a t f = mdecl $ Ldflda p a t f

ldftn :: PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
ldftn p a t m ps = mdecl $ Ldftn p a t m ps

ldind_i :: MethodDecl
ldind_i = mdecl $ Ldind_i

ldind_i1 :: MethodDecl
ldind_i1 = mdecl $ Ldind_i1

ldind_i2 :: MethodDecl
ldind_i2 = mdecl $ Ldind_i2

ldind_i4 :: MethodDecl
ldind_i4 = mdecl $ Ldind_i4

ldind_i8 :: MethodDecl
ldind_i8 = mdecl $ Ldind_i8

ldind_r4 :: MethodDecl
ldind_r4 = mdecl $ Ldind_r4

ldind_r8 :: MethodDecl
ldind_r8 = mdecl $ Ldind_r8

ldind_ref :: MethodDecl
ldind_ref = mdecl $ Ldind_ref

ldind_u1 :: MethodDecl
ldind_u1 = mdecl $ Ldind_u1

ldind_u2 :: MethodDecl
ldind_u2 = mdecl $ Ldind_u2

ldind_u4 :: MethodDecl
ldind_u4 = mdecl $ Ldind_u4

ldloc :: Offset -> MethodDecl
ldloc 0 = mdecl $ Ldloc_0
ldloc 1 = mdecl $ Ldloc_1
ldloc 2 = mdecl $ Ldloc_2
ldloc 3 = mdecl $ Ldloc_3
ldloc x = mdecl $ Ldloc x

ldlocN :: LocalName -> MethodDecl
ldlocN nm = mdecl $ LdlocN nm

ldloca :: Offset -> MethodDecl
ldloca = mdecl . Ldloca

ldlocaN :: LocalName -> MethodDecl
ldlocaN nm = mdecl $ LdlocaN nm

ldsfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldsfld p a t f = mdecl $ Ldsfld p a t f

ldsflda :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldsflda p a t f = mdecl $ Ldsflda p a t f

ldstr :: String -> MethodDecl
ldstr = mdecl . Ldstr

mul :: MethodDecl
mul = mdecl $ Mul

neg :: MethodDecl
neg = mdecl $ Neg

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: AssemblyName -> TypeName -> [PrimitiveType] -> MethodDecl
newobj a t ps = mdecl $ Newobj Void a t ps

nop :: MethodDecl
nop = mdecl $ Nop

pop :: MethodDecl
pop = mdecl $ Pop

rem :: MethodDecl
rem = mdecl $ Rem

ret :: MethodDecl
ret = mdecl $ Ret

stfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
stfld p a t f = mdecl $ Stfld p a t f

stind_i :: MethodDecl
stind_i = mdecl $ Stind_i

stind_i1 :: MethodDecl
stind_i1 = mdecl $ Stind_i1

stind_i2 :: MethodDecl
stind_i2 = mdecl $ Stind_i2

stind_i4 :: MethodDecl
stind_i4 = mdecl $ Stind_i4

stind_i8 :: MethodDecl
stind_i8 = mdecl $ Stind_i8

stind_r4 :: MethodDecl
stind_r4 = mdecl $ Stind_r4

stind_r8 :: MethodDecl
stind_r8 = mdecl $ Stind_r8

stind_ref :: MethodDecl
stind_ref = mdecl $ Stind_ref

stloc :: Offset -> MethodDecl
stloc 0 = mdecl $ Stloc_0
stloc 1 = mdecl $ Stloc_1
stloc 2 = mdecl $ Stloc_2
stloc 3 = mdecl $ Stloc_3
stloc x = mdecl $ Stloc x

stlocN :: LocalName -> MethodDecl
stlocN nm = mdecl $ StlocN nm

stsfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
stsfld p a t f = mdecl $ Stsfld p a t f

sub :: MethodDecl
sub = mdecl $ Sub

tail :: MethodDecl
tail = mdecl $ Tail

tailcall :: MethodDecl -> MethodDecl
tailcall (Instr (OpCode oc)) = Instr (OpCode (Tailcall oc))
tailcall _                   = error $ "Language.Cil.Build.tailcall: Can't tailcall supplied argument"


-- Helper functions

mdecl :: OpCode -> MethodDecl
mdecl i = Instr $ OpCode i

-- Convenient AST functions

-- | Relabel a labelled mdecl with a new label.
label :: Label -> MethodDecl -> MethodDecl
label l (Instr (LabOpCode _ oc)) = Instr $ LabOpCode l oc
label l (Instr (OpCode oc))      = Instr $ LabOpCode l oc
label _ _                        = error $ "Language.Cil.Build.label: "
                                     ++ "Can't label non-Instrs."

comment :: String -> MethodDecl
comment s = Comment s

extends :: TypeName -> Maybe TypeSpec
extends nm = Just (TypeSpec nm)

noExtends :: Maybe TypeSpec
noExtends = Nothing

noImplements :: [TypeSpec]
noImplements = []

classDef :: [ClassAttr] -> TypeName -> Maybe TypeSpec -> [TypeSpec] -> [FieldDef] -> [MethodDef]-> [TypeDef] -> TypeDef
classDef cas n et its fs ms ts = Class cas n et its (map FieldDef fs ++ map MethodDef ms ++ map TypeDef ts)

defaultCtor :: [Parameter] -> MethodDef
defaultCtor = extendsCtor "" "object"

extendsCtor :: AssemblyName -> TypeName -> [Parameter] -> MethodDef
extendsCtor a c ps = Constructor [MaPublic] Void ps
  $ ldarg 0
  : map ldarg [1 .. length ps]
  ++
  [ call [CcInstance] Void a c ".ctor" (map (\(Param _ t _) -> t) ps)
  , ret
  ]

-- | Create a simple Assembly with one method containing the provided MethodDecls.
simpleAssembly :: [MethodDecl] -> Assembly
simpleAssembly ocs = Assembly [mscorlibRef] "DefaultAssemblyName"
  [ Class [CaPublic] "DefaultClassName" Nothing []
    [ MethodDef
      $ Method [MaStatic, MaPublic] Void "DefaultMethodName" [] (entryPoint : ocs)
    ]
  ]

mscorlibRef :: AssemblyRef
mscorlibRef = AssemblyRef "mscorlib" (2, 0, 0 ,0) "B7 7A 5C 56 19 34 E0 89"

