-- |
-- Analysis functions over the Cil AST.
--

module Language.Cil.Analysis (
    opcodes
  ) where

import Language.Cil.Syntax

class Ast a where
  -- A concatenated list of all opcodes.
  opcodes :: a -> [OpCode]

instance Ast Assembly where
  opcodes (Assembly _ _ td) = concatMap opcodes td

instance Ast TypeDef where
  opcodes (Class _ _ _ _ cd)      = concatMap opcodes cd
  opcodes (GenericClass _ _ _ cd) = concatMap opcodes cd

instance Ast ClassDecl where
  opcodes (FieldDef  _)  = []
  opcodes (MethodDef md) = opcodes md
  opcodes (TypeDef td)   = opcodes td

instance Ast MethodDef where
  opcodes (Constructor _ _ _ md) = [ o | OpCode o <- md ]
  opcodes (Method _ _ _ _ md)    = [ o | OpCode o <- md ]

