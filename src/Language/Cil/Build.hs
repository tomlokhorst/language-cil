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
  , and
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
  , ckfinite
  , cle
  , clt 
  , dup
  , isinst
  , ldarg
  , ldargN
  , ldc_i4
  , ldc_i8
  , ldc_r4
  , ldc_r8
  , ldchar
  , ldelem_i
  , ldelem_i1
  , ldelem_i2
  , ldelem_i4
  , ldelem_i8
  , ldelem_u1
  , ldelem_u2
  , ldelem_u4
  , ldelem_u8
  , ldelem_r4
  , ldelem_r8
  , ldelem_ref
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
  , ldlen
  , ldloc
  , ldlocN
  , ldloca
  , ldlocaN
  , ldnull
  , ldsfld
  , ldsflda
  , ldstr
  , mul
  , neg
  , newarr
  , newobj
  , nop
  , not
  , or
  , pop
  , rem
  , ret
  , shl
  , shr
  , shr_un
  , stelem_i
  , stelem_i1
  , stelem_i2
  , stelem_i4
  , stelem_i8
  , stelem_r4
  , stelem_r8
  , stelem_ref
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
  , unaligned
  , unbox
  , xor

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
import Prelude hiding (rem, tail, and, or, not)
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

and :: MethodDecl
and = mdecl $ And

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

ckfinite :: MethodDecl
ckfinite = mdecl $ Ckfinite

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

ldc_i4 :: Integer -> MethodDecl
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
                      then Ldc_i4_s (fromInteger x)
                      else Ldc_i4 x

ldc_i8 :: Integer -> MethodDecl
ldc_i8 = mdecl . Ldc_i8

ldc_r4 :: Float -> MethodDecl
ldc_r4 = mdecl . Ldc_r4

ldc_r8 :: Double -> MethodDecl
ldc_r8 = mdecl . Ldc_r8

ldchar :: Char -> MethodDecl
ldchar c = ldc_i4 (toInteger $ ord c)

ldelem_i :: MethodDecl
ldelem_i = mdecl $ Ldelem_i

ldelem_i1 :: MethodDecl
ldelem_i1 = mdecl $ Ldelem_i1

ldelem_i2 :: MethodDecl
ldelem_i2 = mdecl $ Ldelem_i2

ldelem_i4 :: MethodDecl
ldelem_i4 = mdecl $ Ldelem_i4

ldelem_i8 :: MethodDecl
ldelem_i8 = mdecl $ Ldelem_i8

ldelem_u1 :: MethodDecl
ldelem_u1 = mdecl $ Ldelem_u1

ldelem_u2 :: MethodDecl
ldelem_u2 = mdecl $ Ldelem_u2

ldelem_u4 :: MethodDecl
ldelem_u4 = mdecl $ Ldelem_u4

ldelem_u8 :: MethodDecl
ldelem_u8 = mdecl $ Ldelem_u8

ldelem_r4 :: MethodDecl
ldelem_r4 = mdecl $ Ldelem_r4

ldelem_r8 :: MethodDecl
ldelem_r8 = mdecl $ Ldelem_r8

ldelem_ref :: MethodDecl
ldelem_ref = mdecl $ Ldelem_ref

ldelema :: MethodDecl
ldelema = mdecl $ Ldelema

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

ldlen :: MethodDecl
ldlen = mdecl $ Ldlen

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

ldnull :: MethodDecl
ldnull = mdecl $ Ldnull

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

newarr :: PrimitiveType -> MethodDecl
newarr t = mdecl $ Newarr t

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: AssemblyName -> TypeName -> [PrimitiveType] -> MethodDecl
newobj a t ps = mdecl $ Newobj Void a t ps

nop :: MethodDecl
nop = mdecl $ Nop

not :: MethodDecl
not = mdecl $ Not

or :: MethodDecl
or = mdecl $ Or

pop :: MethodDecl
pop = mdecl $ Pop

rem :: MethodDecl
rem = mdecl $ Rem

ret :: MethodDecl
ret = mdecl $ Ret

shl :: MethodDecl
shl = mdecl $ Shl

shr :: MethodDecl
shr = mdecl $ Shr

shr_un :: MethodDecl
shr_un = mdecl $ Shr_un

stelem_i :: MethodDecl
stelem_i = mdecl $ Stelem_i

stelem_i1 :: MethodDecl
stelem_i1 = mdecl $ Stelem_i1

stelem_i2 :: MethodDecl
stelem_i2 = mdecl $ Stelem_i2

stelem_i4 :: MethodDecl
stelem_i4 = mdecl $ Stelem_i4

stelem_i8 :: MethodDecl
stelem_i8 = mdecl $ Stelem_i8

stelem_r4 :: MethodDecl
stelem_r4 = mdecl $ Stelem_r4

stelem_r8 :: MethodDecl
stelem_r8 = mdecl $ Stelem_r8

stelem_ref :: MethodDecl
stelem_ref = mdecl $ Stelem_ref

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

unaligned :: Alignment -> MethodDecl -> MethodDecl
unaligned a (Instr (OpCode oc)) | supportsUnaligned oc = Instr (OpCode (Unaligned a oc))
unaligned _ _                                          = error $ "Language.Cil.Build.unaligned: Supplied argument doesn't require alignment"

xor :: MethodDecl
xor = mdecl $ Xor

-- Helper functions

mdecl :: OpCode -> MethodDecl
mdecl i = Instr $ OpCode i

supportsUnaligned :: OpCode -> Bool
supportsUnaligned Ldind_i   = True
supportsUnaligned Ldind_i1  = True
supportsUnaligned Ldind_i2  = True
supportsUnaligned Ldind_i4  = True
supportsUnaligned Ldind_i8  = True
supportsUnaligned Ldind_r4  = True
supportsUnaligned Ldind_r8  = True
supportsUnaligned Ldind_ref = True
supportsUnaligned Ldind_u1  = True
supportsUnaligned Ldind_u2  = True
supportsUnaligned Ldind_u4  = True
supportsUnaligned Stind_i   = True
supportsUnaligned Stind_i1  = True
supportsUnaligned Stind_i2  = True
supportsUnaligned Stind_i4  = True
supportsUnaligned Stind_i8  = True
supportsUnaligned Stind_r4  = True
supportsUnaligned Stind_r8  = True
supportsUnaligned Stind_ref = True
supportsUnaligned (Ldfld _ _ _ _) = True
supportsUnaligned (Stfld _ _ _ _) = True
-- there are several cases for not-yet-supported opcodes
-- supportsUnaligned (Ldobj ...)
-- supportsUnaligned (Stobj ...)
-- supportsUnaligned (Initblk ...)
-- supportsUnaligned (Cpblk ...)
supportsUnaligned _         = False



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

