-- |
-- A simple abstraction over the Common Intermediate Language (also known as
-- MSIL - Microsoft Intermediate Language).
-- Currently, it only exposes a small subset of CIL.
--

module Language.Cil
  ( module Language.Cil.Analysis
  , module Language.Cil.Build
  , module Language.Cil.Pretty
  , module Language.Cil.Syntax
  , writeAssembly
  ) where

import Control.Monad (liftM)

import Language.Cil.Analysis
import Language.Cil.Build
import Language.Cil.Pretty
import Language.Cil.Syntax

writeAssembly :: FilePath -> Assembly -> IO ()
writeAssembly fp a = writeFile fp (pr a "")

