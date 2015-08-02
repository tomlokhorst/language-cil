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

  -- * OpCode functions
  , add
  , add_ovf
  , add_ovf_un
  , and
  , beq
  , bge
  , bgt
  , ble
  , blt
  , box
  , br
  , break
  , brfalse
  , brtrue
  , call
  , callvirt
  , castclass
  , ceq
  , cgt
  , ckfinite
  , clt 
  , dup
  , div
  , div_un
  , isinst
  , ldarg
  , ldargN
  , ldarga
  , ldargaN
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
  , ldelema
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
  , mul_ovf
  , mul_ovf_un
  , neg
  , newarr
  , newobj
  , nop
  , not
  , or
  , pop
  , rem
  , rem_un
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
  , sub_ovf
  , sub_ovf_un
  , tail
  , tailcall
  , throw
  , unaligned
  , unalignedPtr
  , unbox
  , unbox_any
  , volatile
  , volatilePtr
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

-- Ambiguous occurences of functions can be resolved when by importing this
-- module qualified, or by hiding Prelude functions.
import Prelude hiding (rem, tail, and, or, not, break, div)
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


-- OpCode functions

add :: MethodDecl
add = OpCode $ Add

add_ovf :: MethodDecl
add_ovf = OpCode $ Add_ovf

add_ovf_un :: MethodDecl
add_ovf_un = OpCode $ Add_ovf_un

and :: MethodDecl
and = OpCode $ And

beq :: Label -> MethodDecl
beq = OpCode . Beq

bge :: Label -> MethodDecl
bge = OpCode . Bge

bgt :: Label -> MethodDecl
bgt = OpCode . Bgt

ble :: Label -> MethodDecl
ble = OpCode . Ble

blt :: Label -> MethodDecl
blt = OpCode . Blt

box :: PrimitiveType -> MethodDecl
box = OpCode . Box

unbox :: PrimitiveType -> MethodDecl
unbox = OpCode . Unbox

unbox_any :: PrimitiveType -> MethodDecl
unbox_any = OpCode . Unbox_any

br :: Label -> MethodDecl
br = OpCode . Br

break :: MethodDecl
break = OpCode $ Break

brfalse :: Label -> MethodDecl
brfalse = OpCode . Brfalse

brtrue :: Label -> MethodDecl
brtrue = OpCode . Brtrue

call :: [CallConv] -> PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
call ccs p l t m ps = OpCode $ Call ccs p l t m ps

callvirt :: PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
callvirt p l t m ps = OpCode $ CallVirt p l t m ps

castclass :: PrimitiveType -> MethodDecl
castclass = OpCode . Castclass

ceq, cgt, clt :: MethodDecl
ceq = OpCode $ Ceq
cgt = OpCode $ Cgt
clt = OpCode $ Clt

ckfinite :: MethodDecl
ckfinite = OpCode $ Ckfinite

dup :: MethodDecl
dup = OpCode $ Dup

div :: MethodDecl
div = OpCode $ Div

div_un :: MethodDecl
div_un = OpCode $ Div_un

isinst :: TypeName -> MethodDecl
isinst = OpCode . Isinst

ldarg :: Offset -> MethodDecl
ldarg 0 = OpCode $ Ldarg_0
ldarg 1 = OpCode $ Ldarg_1
ldarg 2 = OpCode $ Ldarg_2
ldarg 3 = OpCode $ Ldarg_3
ldarg x = OpCode $ Ldarg x

ldargN :: DottedName -> MethodDecl
ldargN = OpCode . LdargN

ldarga :: Offset -> MethodDecl
ldarga = OpCode . Ldarga

ldargaN :: DottedName -> MethodDecl
ldargaN = OpCode . LdargaN

ldc_i4 :: Integer -> MethodDecl
ldc_i4 (-1) = OpCode $ Ldc_i4_m1
ldc_i4 0    = OpCode $ Ldc_i4_0
ldc_i4 1    = OpCode $ Ldc_i4_1
ldc_i4 2    = OpCode $ Ldc_i4_2
ldc_i4 3    = OpCode $ Ldc_i4_3
ldc_i4 4    = OpCode $ Ldc_i4_4
ldc_i4 5    = OpCode $ Ldc_i4_5
ldc_i4 6    = OpCode $ Ldc_i4_6
ldc_i4 7    = OpCode $ Ldc_i4_7
ldc_i4 8    = OpCode $ Ldc_i4_8
ldc_i4 x    = OpCode $ if -127 <= x && x <= 128
                      then Ldc_i4_s (fromInteger x)
                      else Ldc_i4 x

ldc_i8 :: Integer -> MethodDecl
ldc_i8 = OpCode . Ldc_i8

ldc_r4 :: Float -> MethodDecl
ldc_r4 = OpCode . Ldc_r4

ldc_r8 :: Double -> MethodDecl
ldc_r8 = OpCode . Ldc_r8

ldchar :: Char -> MethodDecl
ldchar c = ldc_i4 (toInteger $ ord c)

ldelem_i :: MethodDecl
ldelem_i = OpCode $ Ldelem_i

ldelem_i1 :: MethodDecl
ldelem_i1 = OpCode $ Ldelem_i1

ldelem_i2 :: MethodDecl
ldelem_i2 = OpCode $ Ldelem_i2

ldelem_i4 :: MethodDecl
ldelem_i4 = OpCode $ Ldelem_i4

ldelem_i8 :: MethodDecl
ldelem_i8 = OpCode $ Ldelem_i8

ldelem_u1 :: MethodDecl
ldelem_u1 = OpCode $ Ldelem_u1

ldelem_u2 :: MethodDecl
ldelem_u2 = OpCode $ Ldelem_u2

ldelem_u4 :: MethodDecl
ldelem_u4 = OpCode $ Ldelem_u4

ldelem_u8 :: MethodDecl
ldelem_u8 = OpCode $ Ldelem_u8

ldelem_r4 :: MethodDecl
ldelem_r4 = OpCode $ Ldelem_r4

ldelem_r8 :: MethodDecl
ldelem_r8 = OpCode $ Ldelem_r8

ldelem_ref :: MethodDecl
ldelem_ref = OpCode $ Ldelem_ref

ldelema :: MethodDecl
ldelema = OpCode $ Ldelema

ldfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldfld p a t f = OpCode $ Ldfld p a t f

ldflda :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldflda p a t f = OpCode $ Ldflda p a t f

ldftn :: PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> MethodDecl
ldftn p a t m ps = OpCode $ Ldftn p a t m ps

ldind_i :: MethodDecl
ldind_i = OpCode $ Ldind_i

ldind_i1 :: MethodDecl
ldind_i1 = OpCode $ Ldind_i1

ldind_i2 :: MethodDecl
ldind_i2 = OpCode $ Ldind_i2

ldind_i4 :: MethodDecl
ldind_i4 = OpCode $ Ldind_i4

ldind_i8 :: MethodDecl
ldind_i8 = OpCode $ Ldind_i8

ldind_r4 :: MethodDecl
ldind_r4 = OpCode $ Ldind_r4

ldind_r8 :: MethodDecl
ldind_r8 = OpCode $ Ldind_r8

ldind_ref :: MethodDecl
ldind_ref = OpCode $ Ldind_ref

ldind_u1 :: MethodDecl
ldind_u1 = OpCode $ Ldind_u1

ldind_u2 :: MethodDecl
ldind_u2 = OpCode $ Ldind_u2

ldind_u4 :: MethodDecl
ldind_u4 = OpCode $ Ldind_u4

ldlen :: MethodDecl
ldlen = OpCode $ Ldlen

ldloc :: Offset -> MethodDecl
ldloc 0 = OpCode $ Ldloc_0
ldloc 1 = OpCode $ Ldloc_1
ldloc 2 = OpCode $ Ldloc_2
ldloc 3 = OpCode $ Ldloc_3
ldloc x = OpCode $ Ldloc x

ldlocN :: LocalName -> MethodDecl
ldlocN nm = OpCode $ LdlocN nm

ldloca :: Offset -> MethodDecl
ldloca = OpCode . Ldloca

ldlocaN :: LocalName -> MethodDecl
ldlocaN nm = OpCode $ LdlocaN nm

ldnull :: MethodDecl
ldnull = OpCode $ Ldnull

ldsfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldsfld p a t f = OpCode $ Ldsfld p a t f

ldsflda :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
ldsflda p a t f = OpCode $ Ldsflda p a t f

ldstr :: String -> MethodDecl
ldstr = OpCode . Ldstr

mul :: MethodDecl
mul = OpCode $ Mul

mul_ovf :: MethodDecl
mul_ovf = OpCode $ Mul_ovf

mul_ovf_un :: MethodDecl
mul_ovf_un = OpCode $ Mul_ovf_un

neg :: MethodDecl
neg = OpCode $ Neg

newarr :: PrimitiveType -> MethodDecl
newarr t = OpCode $ Newarr t

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: AssemblyName -> TypeName -> [PrimitiveType] -> MethodDecl
newobj a t ps = OpCode $ Newobj Void a t ps

nop :: MethodDecl
nop = OpCode $ Nop

not :: MethodDecl
not = OpCode $ Not

or :: MethodDecl
or = OpCode $ Or

pop :: MethodDecl
pop = OpCode $ Pop

rem :: MethodDecl
rem = OpCode $ Rem

rem_un :: MethodDecl
rem_un = OpCode $ Rem_un

ret :: MethodDecl
ret = OpCode $ Ret

shl :: MethodDecl
shl = OpCode $ Shl

shr :: MethodDecl
shr = OpCode $ Shr

shr_un :: MethodDecl
shr_un = OpCode $ Shr_un

stelem_i :: MethodDecl
stelem_i = OpCode $ Stelem_i

stelem_i1 :: MethodDecl
stelem_i1 = OpCode $ Stelem_i1

stelem_i2 :: MethodDecl
stelem_i2 = OpCode $ Stelem_i2

stelem_i4 :: MethodDecl
stelem_i4 = OpCode $ Stelem_i4

stelem_i8 :: MethodDecl
stelem_i8 = OpCode $ Stelem_i8

stelem_r4 :: MethodDecl
stelem_r4 = OpCode $ Stelem_r4

stelem_r8 :: MethodDecl
stelem_r8 = OpCode $ Stelem_r8

stelem_ref :: MethodDecl
stelem_ref = OpCode $ Stelem_ref

stfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
stfld p a t f = OpCode $ Stfld p a t f

stind_i :: MethodDecl
stind_i = OpCode $ Stind_i

stind_i1 :: MethodDecl
stind_i1 = OpCode $ Stind_i1

stind_i2 :: MethodDecl
stind_i2 = OpCode $ Stind_i2

stind_i4 :: MethodDecl
stind_i4 = OpCode $ Stind_i4

stind_i8 :: MethodDecl
stind_i8 = OpCode $ Stind_i8

stind_r4 :: MethodDecl
stind_r4 = OpCode $ Stind_r4

stind_r8 :: MethodDecl
stind_r8 = OpCode $ Stind_r8

stind_ref :: MethodDecl
stind_ref = OpCode $ Stind_ref

stloc :: Offset -> MethodDecl
stloc 0 = OpCode $ Stloc_0
stloc 1 = OpCode $ Stloc_1
stloc 2 = OpCode $ Stloc_2
stloc 3 = OpCode $ Stloc_3
stloc x = OpCode $ Stloc x

stlocN :: LocalName -> MethodDecl
stlocN nm = OpCode $ StlocN nm

stsfld :: PrimitiveType -> AssemblyName -> TypeName -> FieldName -> MethodDecl
stsfld p a t f = OpCode $ Stsfld p a t f

sub :: MethodDecl
sub = OpCode $ Sub

sub_ovf :: MethodDecl
sub_ovf = OpCode $ Sub_ovf

sub_ovf_un :: MethodDecl
sub_ovf_un = OpCode $ Sub_ovf_un

tail :: MethodDecl
tail = OpCode $ Tail

tailcall :: MethodDecl -> MethodDecl
tailcall (OpCode oc) = OpCode (Tailcall oc)
tailcall _           = error $ "Language.Cil.Build.tailcall: Can't tailcall supplied argument"

throw :: MethodDecl
throw = OpCode $ Throw

unaligned :: Alignment -> MethodDecl
unaligned a = OpCode $ Unaligned a

unalignedPtr :: Alignment -> MethodDecl -> MethodDecl
unalignedPtr a (OpCode oc) | supportsUnaligned oc = OpCode $ UnalignedPtr a oc
unalignedPtr _ _                                  = error $ "Language.Cil.Build.unalignedPtr: Supplied argument doesn't require alignment"

volatile :: MethodDecl
volatile = OpCode $ Volatile

volatilePtr :: MethodDecl -> MethodDecl
volatilePtr (OpCode oc) | supportsVolatile oc = OpCode $ VolatilePtr oc
volatilePtr _                                 = error $ "Language.Cil.Build.volatilePtr: Supplied argument cannot be marked volatile"

xor :: MethodDecl
xor = OpCode $ Xor

-- Helper functions

supportsUnaligned :: OpCode -> Bool
supportsUnaligned (VolatilePtr oc) = supportsPrefix oc
supportsUnaligned oc               = supportsPrefix oc

supportsVolatile :: OpCode -> Bool
supportsVolatile (UnalignedPtr _ oc) = supportsPrefix oc
supportsVolatile oc                  = supportsPrefix oc

supportsPrefix :: OpCode -> Bool
supportsPrefix Ldind_i   = True
supportsPrefix Ldind_i1  = True
supportsPrefix Ldind_i2  = True
supportsPrefix Ldind_i4  = True
supportsPrefix Ldind_i8  = True
supportsPrefix Ldind_r4  = True
supportsPrefix Ldind_r8  = True
supportsPrefix Ldind_ref = True
supportsPrefix Ldind_u1  = True
supportsPrefix Ldind_u2  = True
supportsPrefix Ldind_u4  = True
supportsPrefix Stind_i   = True
supportsPrefix Stind_i1  = True
supportsPrefix Stind_i2  = True
supportsPrefix Stind_i4  = True
supportsPrefix Stind_i8  = True
supportsPrefix Stind_r4  = True
supportsPrefix Stind_r8  = True
supportsPrefix Stind_ref = True
supportsPrefix (Ldfld _ _ _ _) = True
supportsPrefix (Stfld _ _ _ _) = True
-- there are several cases for not-yet-supported opcodes
-- supportsPrefix (Ldobj ...)
-- supportsPrefix (Stobj ...)
-- supportsPrefix (Initblk ...)
-- supportsPrefix (Cpblk ...)
supportsPrefix _         = False



-- Convenient AST functions

-- | Relabel a labelled MethodDecl with a new label.
label :: Label -> MethodDecl
label l = Label l

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

