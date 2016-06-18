-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Jayant Malani"
myEmail = "jmalani@eng.ucsd.edu"
mySID   = "A53102766"


-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:

-- Foldl takes 3 arguments function f , base - b and list - L. From the list it takes the rightmost element 'e' and applies the function as f(b,e). It left paranthesises elements of the list. For example if operator is (+) foldl of foldl (+) 0 [1, 2, 3, 4, 5] will return (((((0 + 1) + 2) + 3) + 4) + 5) and then evaluates the whole expression. Foldr the paranthesises would begin from right.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b [] = b
myFoldl f b (x:xs) = myFoldl f (f b x) xs  

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse xs = Prelude.foldl f [] xs
          where f a b = b:a 

-- 3. Define `foldr` in terms of `foldl`:

rFunc f e r = f r e
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = Prelude.foldl (rFunc f) b (myReverse xs) 

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = Prelude.foldr (rFunc f) b (myReverse xs) 

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?

-- Since foldl and foldl' are the same except for their strictness properties, so if both return a result, it must be the same. foldl' is the more efficient way to arrive at that result because it doesn't build a huge thunk. However, if the combining function is lazy in its first argument, foldl may happily return a result where foldl' hits an exception 

-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show, Eq)

-- Define a `delete` function for BSTs of this type:

-- helper function 
successor (Bind k v l r) 
        | l==Emp    = (k,v)
        | otherwise = successor l
 
delete :: (Eq v, Ord k) => k -> BST k v -> BST k v
delete k (Bind k' v' l r)
  | k == k'      =  if (l == Emp && r == Emp) then Emp
                    else if (l == Emp) then r
                    else if (r == Emp) then l
                    else Bind (fst (successor r))  (snd (successor r)) l (Hw2.delete (fst (successor r)) r)
  | k < k'       =  Bind k' v' (Hw2.delete k l) r
  | otherwise    =  Bind k' v' l (Hw2.delete k r)                    
 

-- Part 3: An Interpreter for WHILE
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as

type Variable = String

-- Programs in the language are simply values of the type

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value`

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return
-- the value `0`. In future assignments, we will add this as a
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State`
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function

evalE :: Expression -> State Store Value

evalE (Var a)       = do s <- get
                         let t = Data.Map.lookup a s
                         case t of
                           Nothing -> return (IntVal 0)
                           Just  k -> return k
evalE (Val a)       = return a
evalE (Op op l r)   = do l' <- evalE l
                         r' <- evalE r
                         return (operate op l' r') 

-- helper function to evaluate operator						 
funcOp op (IntVal l) (IntVal r) = IntVal (l `op` r)
funcOp _ _ _                    = IntVal 0

-- helper function to evaluate bool operator						                        
funcBop op (IntVal l) (IntVal r) = BoolVal (l `op` r)
funcBop _ _ _                    = BoolVal False

-- helper function to pass operators 
operate Plus    = funcOp (+)
operate Minus   = funcOp (-)
operate Times   = funcOp (*)
operate Divide  = funcOp div 

operate Gt  = funcBop (>)
operate Ge  = funcBop (>=)
operate Lt  = funcBop (<)
operate Le  = funcBop (<=)

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract
-- the value of the "current store" in a variable `s` use `s <- get`.

-- evalOp :: Bop -> Value -> Value -> Value
-- evalOp Plus (IntVal i) (IntVal j) = IntVal (i+j)
-- 
-- -- >
-- 
-- evalE (Var x)      = error "TBD"
-- evalE (Val v)      = error "TBD"
-- evalE (Op o e1 e2) = error "TBD"

-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.

-- evaluate string of different types
evalS (Assign x e )    = do 
                            s<- get
                            e'<- evalE e
                            put (Data.Map.insert x e' s)
                            
evalS w@(While e s)    = do
                            e'<- evalE e
                            case e' of
                                BoolVal True -> do
                                                    evalS s
                                                    evalS w
                                _            -> evalS Skip
evalS Skip             = return ()

evalS (Sequence s1 s2) = do
                            evalS s1
                            evalS s2
evalS (If e s1 s2)     = do
                            e'<-evalE e
                            case e' of
                                BoolVal True  ->     evalS s1
                                BoolVal False ->     evalS s2
                                IntVal _      ->     evalS Skip
 
-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function

execS :: Statement -> Store -> Store
execS stmt store = execState (evalS stmt) store

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`.
-- **Hint:** You may want to use the library function

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will
-- "run" a statement starting with the `empty` store (where no
-- variable is initialized). Running the program should print
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = try intP <|> try boolP

-- To do so, fill in the implementations of

-- helper functions 
intP :: Parser Value
intP = do skipMany space
          num <- many1 digit
          return (IntVal(read num :: Int))

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do skipMany space
                string s
                return x

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = try (constP "true" (BoolVal True)) <|> try (constP "false" (BoolVal False))

-- Continue to use the above to parse the binary operators

addopP::Parser Bop
addopP = try (constP "+" Plus) <|> try (constP "-" Minus)

mulopP::Parser Bop
mulopP = try (constP "*" Times) <|> try (constP "/" Divide)

relopP::Parser Bop
relopP= try(constP ">" Gt) <|> try (constP ">=" Gt) <|> try (constP "<" Lt) <|> try (constP "<=" Le)

-- opP :: Parser Bop
-- opP = try addopP <|> try mulopP <|> try relopP

opP :: Parser Bop 
opP =   try (constP "+" Plus)
    <|> try (constP "-" Minus)
    <|> try (constP "*" Times)
    <|> try (constP "/" Divide)
    <|> try (constP ">" Gt)
    <|> try (constP ">=" Ge)
    <|> try (constP "<" Lt)
    <|> try (constP "<=" Le)


-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = do skipMany space
          many1 upper

-- Use the above to write a parser for `Expression` values
        
exprP :: Parser Expression
exprP =  try funcExpP <|> try parExprP <|> try valVExpP

-- helper function to evaluate the parsing 
parExprP :: Parser Expression
parExprP = do char '('
              spaces
              e <- exprP
              spaces
              char ')'
              return e

-- evaluate the function with variable and expressions
varExprP :: Parser Expression
varExprP = liftM Var varP

valExprP :: Parser Expression
valExprP = liftM Val valueP

valVExpP :: Parser Expression
valVExpP =   try varExprP
            <|> try valExprP

funcExpP :: Parser Expression
funcExpP = do l <- try valVExpP <|> try parExprP
              spaces
              bop <- opP
              spaces
              r <- exprP
              return $ Op bop l r

-- Use the above to write a parser for `Expression` values

-- exprP :: Parser Expression
-- exprP = error "TBD"

-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser Statement
statementP =   try seqStP <|> try assignP <|> try ifP <|> try whileP <|> try skipP 

-- helper functions to evaluate the different types of functions
skipP :: Parser Statement
skipP = do string "skip"
           return (Skip)

seqStP :: Parser Statement
seqStP = do s1 <- try assignP <|> try ifP <|> try whileP <|> try skipP
            char ';'
            spaces
            s2 <- statementP
            return (Sequence s1 s2)

whileP :: Parser Statement
whileP = do string "while"
            spaces
            e <- exprP
            spaces
            string "do"
            spaces
            s <- statementP
            spaces
            string "endwhile"
            return (While e s)

ifP :: Parser Statement
ifP = do string "if"
         spaces
         e <- exprP
         spaces
         string "then"
         spaces
         t <- statementP
         spaces
         string "else"
         spaces
         f <- statementP
         spaces
         string "endif"
         return (If e t f)

assignP :: Parser Statement
assignP = do v <- varP
             spaces
             string ":="
             spaces
             e <- exprP
             return (Assign v e)

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~





