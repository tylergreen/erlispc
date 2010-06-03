module Parser where
    
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Token
import Monad
import Compiler

pExp :: Parser Exp
pExp = pVar <|>
       pAtom <|>
       pTuple <|>
       pRec <|>
       pApp <|>
       pNum <|>
       pList 


-- only handles integers
pNum = do ns <- many1 digit
          return $ IntConst (read ns)

pVar = do c <- upper
          cs <- many alphaNum
          return $ Var (c:cs)

pAtom = do c <- lower
           cs <- many alphaNum 
           return $ Atom (c:cs)

pTuple = do char '{'
            spaces
            exps <- sepBy pExp spaces
            spaces
            char '}'
            return $ Tuple exps

pList = do char '['
           spaces
           exps <- sepBy pExp spaces
           spaces
           char ']'
           return $ List exps

pRec = do char '('
          spaces
          string "receive"
          spaces
          exps <- sepBy pExp spaces
          spaces   -- bug here: spaces get consumed by aboved (for all paren seqs)
          char ')'
          return $ Receive (pair exps)

pApp = do char '('
          spaces
          exps <- sepBy pExp spaces
          spaces
          char ')'
          return $ App exps

pair [] = []
pair [x] = []
pair (x:y:xs) = (x,y) : pair xs


-- (receive foo bar 



