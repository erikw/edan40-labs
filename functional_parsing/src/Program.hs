module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

{-iter :: Parser a -> Parser [a]  -}
{-iter m = m # iter m >-> cons ! return [] -}

{-type Parser a = String -> Maybe (a, String)-}
-- Parser [Statement.T] ::: String -> Maybe ([Statement.T], String)
-- Parser Program [Statement.T] ::: String -> Maybe (Program [Statement.T], String)


instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = error "Program.toString not implemented"
             
exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input
