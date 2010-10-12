language-cil: Manipulating Common Intermediate Language AST
===========================================================

Language-Cil is a Haskell library for manipulating CIL abstract syntax and
generating .il files using the concrete syntax of CIL.

Common Intermediate Language (CIL), formerly known as Microsoft Intermediate
Language (MSIL), is the lowest level language that runs on the Microsoft .NET
and Mono platforms.

Read more on the Wikipedia page on 
[Common Intermediate Language](http://en.wikipedia.org/wiki/Common_Intermediate_Language).


Current status
--------------

This library is still very much under development.
Only a subset of the full CIL AST is implemented.

A parser for concrete syntax has not yet been implemented.


Module overview
---------------

 - **Language.Cil** Top level module, reexports sub modules
 - **Language.Cil.Analysis** Some analysis functions over AST, not really used
   yet.
 - **Language.Cil.Build** Start constructors and convenience functions, it is
   suggested to use these over raw AST constructors.
 - **Language.Cil.Pretty** Pretty printer function `pr`. Returns a `ShowS`.
 - **Language.Cil.Syntax** Concrete AST data types.


Getting started
---------------

 1. Make sure you have Mono or .NET installed.
 2. Install the library.
 3. Create an AST, eg:
        module Example where
        
        import Language.Cil
        
        main :: IO ()
        main = writeAssembly "Test.il" ass

        ass :: Assembly
        ass = simpleAssembly
          [ nop
             -- Print message to stdout
          , ldstr "Enter an integer and press return."
          , call [] Void "mscorlib" "System.Console" "WriteLine" [String]
          
            -- Reads an integer from stdin, add one two it, print result to stdout.
          , call [] String "mscorlib" "System.Console" "ReadLine" []
          , call [] Int32 "" "int32" "Parse" [String]
          
          , ldc_i4 1
          , add
          , call [] Void "mscorlib" "System.Console" "WriteLine" [Int32]
          
          , ret
          ]
 4. Write the AST to a file by running `main`.
 5. Run the IL assembler: `ilasm Test.il`
 6. Run the generated executable: `mono Test.exe` (or `Test.exe` on Windows)

