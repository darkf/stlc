{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser where
import Data.Maybe (fromMaybe)
import Text.Peggy hiding (space)
import Term

[peggy|
top :: Term = space* expr space* !. { $2 }

lineComment :: () = '--' (!'\n' .)* '\n' { () }
space :: () = [ \r\n\t] { () } / lineComment

term :: Term = '(' expr ')' { $1 }
             / identifier { Var $1 }
             / integer { Const $1 }

expr :: Term = expr space* term { App $1 $3 }
             / '\\' identifier '.' expr { Abs $1 BottomTy $2 }
             / term

identifier ::: String
  = [a-zA-Z_] [a-zA-Z]* { $1 : $2 }

integer ::: Integer
  = [0-9] [0-9]* { read ($1 : $2) }
|]

parseProgram program = parseString top "<input>" program
