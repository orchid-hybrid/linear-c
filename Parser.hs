import Data.Char
import Text.ParserCombinators.ReadP as ReadP
import Control.Applicative
import System.IO

import Linear

brackets p = between (char '(' <* skipSpaces) (char ')') p
mbrackets p = (brackets p) <++ p

parseQ = choice [ do string "lin"; return Lin
                , do string "un"; return Un
                ]

parseQd x = do q <- parseQ
               string ":"
               x' <- x
               return $ x' q

data Top = ValD String T
         | LetD String T T
         | TypeD String Ty
     deriving (Eq)


instance Show Top where
  show (ValD a e) = "val " ++ a ++ " = " ++ (show e)
  show (LetD a e b) = "let " ++ a ++ " = " ++ (show e) ++ " in " ++ (show b)
  show (TypeD a t) =  "type " ++ a ++ " = " ++ (show t)

data TopExp = TopE Top
            | BasicE T
     deriving (Eq)

instance Show TopExp where
  show (TopE x) = show x
  show (BasicE x) = show x

parseD = choice [ parseValD, parseLetD, parseTypeD]

parseValD = do string "val"; skipSpaces
               a <- munch1 isAlpha
               skipSpaces; string "="; skipSpaces
               e <- parseE; skipSpaces
               return (ValD a e)

parseLetD = do string "let"; skipSpaces
               a <- munch1 isAlpha
               skipSpaces; string "="; skipSpaces
               e <- parseE;
               skipSpaces; string "in"; skipSpaces
               s <- parseE;
               return (LetD a e s)

parseTypeA = munch1 isAlpha
parseTypeD = do string "type"; skipSpaces;
                tn <- parseTypeA
                skipSpaces; string "="; skipSpaces
                t <- parseTy
                return (TypeD tn t)

parseApp = skipSpaces >> return (:$)

parseE = mbrackets (chainl1 (mbrackets parseE')  parseApp)
parseE' = choice [ parseIfThenElse
                 , parseLam
                 , parseSplit
                 , parsePair
                 , parseBool
                 ] <++ parseVar

parseBool = do q <- parseQ; skipSpaces
               x <- choice [ do string "true"; return $ Boole q True
                           , do string "false"; return $ Boole q False
                           ]
               return x

parseLam = do q <- parseQ; skipSpaces
              string "\\"
              p <- munch1 isAlpha
              skipSpaces; string ":"; skipSpaces
              t <- parseTy
              skipSpaces; string "."; skipSpaces
              b <- parseE
              return (Lambda q p t b)

parsePair = do q <- parseQ; skipSpaces;
               string "<"; skipSpaces
               f <- parseE; skipSpaces
               string ","; skipSpaces
               s <- parseE; skipSpaces
               string ">"
               return (Pair q f s)

parseVar = do v <- munch1 isAlpha
              return (Var v)

parseIfThenElse = do string "if"; skipSpaces
                     b <- parseE; skipSpaces
                     string "then"; skipSpaces
                     t <- parseE ; skipSpaces
                     string "else"; skipSpaces
                     e <- parseE
                     return (IfThenElse b t e)

parseSplit = do string "split"; skipSpaces
                v <- parseE; skipSpaces
                string "as"; skipSpaces
                sv1 <- munch1 isAlpha; skipSpaces
                string ","; skipSpaces
                sv2 <- munch1 isAlpha; skipSpaces;
                string "in"; skipSpaces
                b <- parseE
                return $ Split v sv1 sv2 b

parsePTy = mbrackets parsePTy'
parsePTy' = choice [ do string "bool"; return $ BooleTy
                   , do t1 <- parseTy
                        skipSpaces; string "*"; skipSpaces
                        t2 <- parseTy
                        return $ t1 :* t2
                   , do t1 <- parseTy
                        skipSpaces; string "->"; skipSpaces
                        t2 <- parseTy
                        return $ t1 :-> t2
                   ]

parseTy = mbrackets parseTy'
parseTy' = do q <- parseQ; skipSpaces
              pty <- parsePTy
              return (q, pty)
 

printall foo = case foo of
               [] -> putStrLn ""
               (x, c):xs -> do putStrLn "---------"
                               mapM_ (putStrLn . show) x
                               printall xs


parseTop = (parseD >>= return . TopE) <++ (parseE >>= return . BasicE)

parseall =  ReadP.many1 (skipSpaces >> parseTop)

main = do
     s <- readFile "tests/t2"
     let x = readP_to_S parseall $ s
     printall $ (filter ((== "") . snd) x)
