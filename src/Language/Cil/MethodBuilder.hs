{-# LANGUAGE TypeSynonymInstances #-}

module Language.Cil.MethodBuilder
  ( MBuilder(..)
  , MethodBuilder
  , buildMethod
  )
where

import Language.Cil.Syntax
import Control.Monad.State
import Data.Set
import Data.Char (ord)
import Prelude hiding (rem, tail, and, or, not, break, div)

class (Monad m) => MBuilder m where
  append :: OpCode -> m ()
  comment :: String -> m ()
  placeLabel :: Label -> m ()
  placeFreshLabel :: m Label
  allocateFreshLabel :: String -> m Label
  freshLocal :: PrimitiveType -> m Offset
  freshLocalN :: String -> PrimitiveType -> m Offset

data MethodBuilderState = MBState 
                            { reversedInstructions :: [MethodDecl]
                            , nextFreshLocal :: Int
                            , nextFreshLabelSuffix :: Int -- eventually replace by just keeping a count of how many bytes we have so far in reversedInstructions
                            , localDefinitions :: [(Offset, Local)]
                            , labelsInUse :: Set Label
                            }

type MethodBuilder = State MethodBuilderState

initialState :: MethodBuilderState
initialState = MBState
                 { reversedInstructions = []
                 , nextFreshLocal = 0
                 , nextFreshLabelSuffix = 0
                 , localDefinitions = []
                 , labelsInUse = empty
                 }

buildMethod :: MethodBuilder a -> [MethodDecl]
buildMethod mb = let (r, s) = runState mb initialState -- there is a bit more to it than this (emit .localsinit from whatever locals we gave out)
                  in reverse $ reversedInstructions s

instance MBuilder MethodBuilder where
  append = append' . OpCode
  comment = append' . Comment
  placeLabel = append' . Label
  placeFreshLabel = do
                      l <- allocateFreshLabel "_label"
                      placeLabel l
                      return l
  allocateFreshLabel l = do
                           suffix <- freshLabelSuffix
                           return (l ++ "_" ++ (show suffix))
  freshLocal t = freshLocal' Nothing t
  freshLocalN n t = freshLocal' (Just n) t

freshLocal' :: Maybe String -> PrimitiveType -> MethodBuilder Offset
freshLocal' n t = do
                    offset <- freshLocalIndex
                    locals <- gets localDefinitions
                    let name = case n of
                                 Just n' | nameInUse locals n' -> n'
                                 Just n'                       -> n' ++ "_" ++ (show offset)
                                 Nothing                       -> "v_" ++ (show offset)
                    -- make a note of the type for later emitting .localsinit
                    let newLocal = Local t name
                    modify (\s -> s { localDefinitions = (offset, newLocal) : locals })
                    return offset
                  where nameInUse ls n = any (\(_, Local _ n') -> n == n') ls

append' :: MethodDecl -> MethodBuilder ()
append' decl = modify (\s -> s { reversedInstructions = decl : (reversedInstructions s) })

freshLocalIndex :: MethodBuilder Offset
freshLocalIndex = do
                    result <- gets nextFreshLocal
                    modify (\s -> s { nextFreshLocal = result + 1 })
                    return result

freshLabelSuffix :: MethodBuilder Int
freshLabelSuffix = do
                     result <- gets nextFreshLabelSuffix
                     modify (\s -> s { nextFreshLabelSuffix = result + 1})
                     return result










-- mdecl functions

add :: (MBuilder m) => m ()
add = append Add

add_ovf :: (MBuilder m) => m ()
add_ovf = append Add_ovf

add_ovf_un :: (MBuilder m) => m ()
add_ovf_un = append Add_ovf_un

and :: (MBuilder m) => m ()
and = append And

beq :: (MBuilder m) => Label -> m ()
beq = append . Beq

bge :: (MBuilder m) => Label -> m ()
bge = append . Bge

bgt :: (MBuilder m) => Label -> m ()
bgt = append . Bgt

ble :: (MBuilder m) => Label -> m ()
ble = append . Ble

blt :: (MBuilder m) => Label -> m ()
blt = append . Blt

box :: (MBuilder m) => PrimitiveType -> m ()
box = append . Box

unbox :: (MBuilder m) => PrimitiveType -> m ()
unbox = append . Unbox

br :: (MBuilder m) => Label -> m ()
br = append . Br

break :: (MBuilder m) => m ()
break = append Break

brfalse :: (MBuilder m) => Label -> m ()
brfalse = append . Brfalse

brtrue :: (MBuilder m) => Label -> m ()
brtrue = append . Brtrue

call :: (MBuilder m) => [CallConv] -> PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> m ()
call ccs p l t m ps = append $ Call ccs p l t m ps

callvirt :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> m ()
callvirt p l t m ps = append $ CallVirt p l t m ps

castclass :: (MBuilder m) => PrimitiveType -> m ()
castclass = append . Castclass

ceq, cge, cgt, cle, clt :: (MBuilder m) => m ()
ceq = append Ceq
cge = append Cge
cgt = append Cgt
cle = append Cle
clt = append Clt

ckfinite :: (MBuilder m) => m ()
ckfinite = append Ckfinite

dup :: (MBuilder m) => m ()
dup = append Dup

div :: (MBuilder m) => m ()
div = append Div

div_un :: (MBuilder m) => m ()
div_un = append Div_un

isinst :: (MBuilder m) => TypeName -> m ()
isinst = append . Isinst

ldarg :: (MBuilder m) => Offset -> m ()
ldarg 0 = append Ldarg_0
ldarg 1 = append Ldarg_1
ldarg 2 = append Ldarg_2
ldarg 3 = append Ldarg_3
ldarg x = append $ Ldarg x

ldargN :: (MBuilder m) => DottedName -> m ()
ldargN = append . LdargN

ldarga :: (MBuilder m) => Offset -> m ()
ldarga = append .Ldarga

ldargaN :: (MBuilder m) => DottedName -> m ()
ldargaN = append . LdargaN

ldc_i4 :: (MBuilder m) => Integer -> m ()
ldc_i4 (-1) = append Ldc_i4_m1
ldc_i4 0    = append Ldc_i4_0
ldc_i4 1    = append Ldc_i4_1
ldc_i4 2    = append Ldc_i4_2
ldc_i4 3    = append Ldc_i4_3
ldc_i4 4    = append Ldc_i4_4
ldc_i4 5    = append Ldc_i4_5
ldc_i4 6    = append Ldc_i4_6
ldc_i4 7    = append Ldc_i4_7
ldc_i4 8    = append Ldc_i4_8
ldc_i4 x    = append $ if -127 <= x && x <= 128
                         then Ldc_i4_s (fromInteger x)
                         else Ldc_i4 x

ldc_i8 :: (MBuilder m) => Integer -> m ()
ldc_i8 = append . Ldc_i8

ldc_r4 :: (MBuilder m) => Float -> m ()
ldc_r4 = append . Ldc_r4

ldc_r8 :: (MBuilder m) => Double -> m ()
ldc_r8 = append . Ldc_r8

ldchar :: (MBuilder m) => Char -> m ()
ldchar c = ldc_i4 (toInteger $ ord c)

ldelem_i :: (MBuilder m) => m ()
ldelem_i = append Ldelem_i

ldelem_i1 :: (MBuilder m) => m ()
ldelem_i1 = append Ldelem_i1

ldelem_i2 :: (MBuilder m) => m ()
ldelem_i2 = append Ldelem_i2

ldelem_i4 :: (MBuilder m) => m ()
ldelem_i4 = append Ldelem_i4

ldelem_i8 :: (MBuilder m) => m ()
ldelem_i8 = append Ldelem_i8

ldelem_u1 :: (MBuilder m) => m ()
ldelem_u1 = append Ldelem_u1

ldelem_u2 :: (MBuilder m) => m ()
ldelem_u2 = append Ldelem_u2

ldelem_u4 :: (MBuilder m) => m ()
ldelem_u4 = append Ldelem_u4

ldelem_u8 :: (MBuilder m) => m ()
ldelem_u8 = append Ldelem_u8

ldelem_r4 :: (MBuilder m) => m ()
ldelem_r4 = append Ldelem_r4

ldelem_r8 :: (MBuilder m) => m ()
ldelem_r8 = append Ldelem_r8

ldelem_ref :: (MBuilder m) => m ()
ldelem_ref = append Ldelem_ref

ldelema :: (MBuilder m) => m ()
ldelema = append Ldelema

ldfld :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
ldfld p a t f = append $ Ldfld p a t f

ldflda :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
ldflda p a t f = append $ Ldflda p a t f

ldftn :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> MethodName -> [PrimitiveType] -> m ()
ldftn p a t m ps = append $ Ldftn p a t m ps

ldind_i :: (MBuilder m) => m ()
ldind_i = append Ldind_i

ldind_i1 :: (MBuilder m) => m ()
ldind_i1 = append Ldind_i1

ldind_i2 :: (MBuilder m) => m ()
ldind_i2 = append Ldind_i2

ldind_i4 :: (MBuilder m) => m ()
ldind_i4 = append Ldind_i4

ldind_i8 :: (MBuilder m) => m ()
ldind_i8 = append Ldind_i8

ldind_r4 :: (MBuilder m) => m ()
ldind_r4 = append Ldind_r4

ldind_r8 :: (MBuilder m) => m ()
ldind_r8 = append Ldind_r8

ldind_ref :: (MBuilder m) => m ()
ldind_ref = append Ldind_ref

ldind_u1 :: (MBuilder m) => m ()
ldind_u1 = append Ldind_u1

ldind_u2 :: (MBuilder m) => m ()
ldind_u2 = append Ldind_u2

ldind_u4 :: (MBuilder m) => m ()
ldind_u4 = append Ldind_u4

ldlen :: (MBuilder m) => m ()
ldlen = append Ldlen

ldloc :: (MBuilder m) => Offset -> m ()
ldloc 0 = append Ldloc_0
ldloc 1 = append Ldloc_1
ldloc 2 = append Ldloc_2
ldloc 3 = append Ldloc_3
ldloc x = append $ Ldloc x

ldlocN :: (MBuilder m) => LocalName -> m ()
ldlocN nm = append $ LdlocN nm

ldloca :: (MBuilder m) => Offset -> m ()
ldloca = append . Ldloca

ldlocaN :: (MBuilder m) => LocalName -> m ()
ldlocaN nm = append $ LdlocaN nm

ldnull :: (MBuilder m) => m ()
ldnull = append Ldnull

ldsfld :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
ldsfld p a t f = append $ Ldsfld p a t f

ldsflda :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
ldsflda p a t f = append $ Ldsflda p a t f

ldstr :: (MBuilder m) => String -> m ()
ldstr = append . Ldstr

mul :: (MBuilder m) => m ()
mul = append Mul

mul_ovf :: (MBuilder m) => m ()
mul_ovf = append Mul_ovf

mul_ovf_un :: (MBuilder m) => m ()
mul_ovf_un = append Mul_ovf_un

neg :: (MBuilder m) => m ()
neg = append Neg

newarr :: (MBuilder m) => PrimitiveType -> m ()
newarr t = append $ Newarr t

-- | Creates a new object.
-- Note that this function assumes the constructor returns Void.
-- If this is not the case, call the Newobj constructor manually.
newobj :: (MBuilder m) => AssemblyName -> TypeName -> [PrimitiveType] -> m ()
newobj a t ps = append $ Newobj Void a t ps

nop :: (MBuilder m) => m ()
nop = append Nop

not :: (MBuilder m) => m ()
not = append Not

or :: (MBuilder m) => m ()
or = append Or

pop :: (MBuilder m) => m ()
pop = append Pop

rem :: (MBuilder m) => m ()
rem = append Rem

rem_un :: (MBuilder m) => m ()
rem_un = append Rem_un

ret :: (MBuilder m) => m ()
ret = append Ret

shl :: (MBuilder m) => m ()
shl = append Shl

shr :: (MBuilder m) => m ()
shr = append Shr

shr_un :: (MBuilder m) => m ()
shr_un = append Shr_un

stelem_i :: (MBuilder m) => m ()
stelem_i = append Stelem_i

stelem_i1 :: (MBuilder m) => m ()
stelem_i1 = append Stelem_i1

stelem_i2 :: (MBuilder m) => m ()
stelem_i2 = append Stelem_i2

stelem_i4 :: (MBuilder m) => m ()
stelem_i4 = append Stelem_i4

stelem_i8 :: (MBuilder m) => m ()
stelem_i8 = append Stelem_i8

stelem_r4 :: (MBuilder m) => m ()
stelem_r4 = append Stelem_r4

stelem_r8 :: (MBuilder m) => m ()
stelem_r8 = append Stelem_r8

stelem_ref :: (MBuilder m) => m ()
stelem_ref = append Stelem_ref

stfld :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
stfld p a t f = append $ Stfld p a t f

stind_i :: (MBuilder m) => m ()
stind_i = append Stind_i

stind_i1 :: (MBuilder m) => m ()
stind_i1 = append Stind_i1

stind_i2 :: (MBuilder m) => m ()
stind_i2 = append Stind_i2

stind_i4 :: (MBuilder m) => m ()
stind_i4 = append Stind_i4

stind_i8 :: (MBuilder m) => m ()
stind_i8 = append Stind_i8

stind_r4 :: (MBuilder m) => m ()
stind_r4 = append Stind_r4

stind_r8 :: (MBuilder m) => m ()
stind_r8 = append Stind_r8

stind_ref :: (MBuilder m) => m ()
stind_ref = append Stind_ref

stloc :: (MBuilder m) => Offset -> m ()
stloc 0 = append Stloc_0
stloc 1 = append Stloc_1
stloc 2 = append Stloc_2
stloc 3 = append Stloc_3
stloc x = append $ Stloc x

stlocN :: (MBuilder m) => LocalName -> m ()
stlocN nm = append $ StlocN nm

stsfld :: (MBuilder m) => PrimitiveType -> AssemblyName -> TypeName -> FieldName -> m ()
stsfld p a t f = append $ Stsfld p a t f

sub :: (MBuilder m) => m ()
sub = append Sub

sub_ovf :: (MBuilder m) => m ()
sub_ovf = append Sub_ovf

sub_ovf_un :: (MBuilder m) => m ()
sub_ovf_un = append Sub_ovf_un

tail :: (MBuilder m) => m ()
tail = append Tail

throw :: (MBuilder m) => m ()
throw = append Throw

unaligned :: (MBuilder m) => Alignment -> m ()
unaligned a = append $ Unaligned a

volatile :: (MBuilder m) => m ()
volatile = append Volatile

xor :: (MBuilder m) => m ()
xor = append Xor

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



