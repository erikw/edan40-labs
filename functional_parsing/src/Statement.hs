module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
    | Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" -# require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin 

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((cond, ifE), thenE) = If cond ifE thenE

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (cond, stmt) = While cond stmt

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read 

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write




exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment var expr : stmts) dict input = exec stmts newDict input
                where newDict = Dictionary.insert (var, Expr.value expr dict) dict 
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin bstmts : stmts) dict input = exec (bstmts ++ stmts) dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec s@(While cond stmt : stmts) dict input = if (Expr.value cond dict) > 0
                                                then exec (stmt : s) dict input
                                                else exec stmts dict input
exec (Read var : stmts) dict (i:is) = exec stmts newDict is
                    where newDict = Dictionary.insert (var, i) dict
exec (Write expr : stmts) dict input = (Expr.value expr dict) : exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifStmt ! while ! readStmt ! write
  toString = error "Statement.toString not implemented" -- TODO how?
