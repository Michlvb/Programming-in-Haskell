import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

return' :: a -> Parser a
return' v = P (\inp -> [(v, inp)])

failure' :: Parser a
failure' = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)])

parse'  :: Parser a -> String -> [(a, String)]
parse' (P p) inp = p inp

sat' :: (Char -> Bool) -> Parser Char
sat' p = do x <- item
            if p x then return' x else failure'

char' :: Char -> Parser Char
char' x = sat'(==x)

digit' :: Parser Char
digit' = sat' isDigit

string'     :: String -> Parser String
string' []    =   return' []
string (x:xs) = do char' x 
                   string' xs
                   return' (x:xs)

nat' :: Parser Int
nat' = do xs <- some digit'
          return (read xs)

int' :: Parser Int
int' = do char' '-'
          n <- nat'
          return' (-n)
         <|> nat'
