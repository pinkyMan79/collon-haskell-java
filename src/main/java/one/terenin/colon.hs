{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Map (Map)
import Data.Bits ((.&.), (.|.))
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Text.Read (readMaybe)
import System.IO (putStrLn, putChar, getLine)
import Control.Exception (try, SomeException)

-- Stack for storing numbers
type Stack = [Int]

-- Definition of a new word
type WordDef = [Command]

-- Type representing command of the Colon language
data Command
  = Push Int
  | Add | Sub | Mul | Div | Mod
  | Dup | Drop | Swap | Over | Rot
  | Emit | CR | Dot | DotString String
  | Equals | Less | Greater | And | Or | Invert
  | If [Command] [Command]
  | DoLoop [Command]
  | I
  | Variable String
  | Set String
  | Get String
  | Constant String Int
  | Word String WordDef
  | Call String
  | Comment
  | NewArray Int
  | Index Int
  | SetIndex Int
  | Begin
  | Until [Command]
  deriving (Show, Eq)

-- Dictionary for storing defined words
type Dictionary = Map String WordDef

-- Program state of Colon
data StateColon = StateColon
  { stack :: Stack
  , dictionary :: Dictionary
  , memory :: Map String Int
  }

-- Monad for handling state and errors
type ColonM = ExceptT String (StateT StateColon IO)

-- Change stack state (moved up)
modifyStack :: (Stack -> Either String Stack) -> ColonM ()
modifyStack f = do
  stack <- gets stack
  case f stack of
    Left err -> throwError err
    Right newStack -> modify $ \s -> s { stack = newStack }

-- Universal function for performing binary operations
binaryOp :: (Int -> Int -> Int) -> ColonM ()
binaryOp op = modifyStack $ \case
  (y : x : rest) -> Right (op x y : rest)
  _ -> Left "Stack underflow"

-- Execution of command
execute :: Command -> ColonM ()
execute (Push n) = modifyStack (Right . (n :))
execute Add = binaryOp (+)
execute Sub = binaryOp (-)
execute Mul = binaryOp (*)
execute Div = binaryOp div
execute Mod = binaryOp mod
execute Equals = binaryOp (\x y -> if x == y then 1 else 0)
execute Less = binaryOp (\x y -> if x < y then 1 else 0)
execute Greater = binaryOp (\x y -> if x > y then 1 else 0)
execute And = binaryOp (.&.)
execute Or = binaryOp (.|.)
execute Invert = modifyStack $ \case
  (x : xs) -> Right ((if x == 0 then 1 else 0) : xs)
  _ -> Left "Stack underflow"
execute Dup = modifyStack $ \case
  (x : xs) -> Right (x : x : xs)
  _ -> Left "Stack underflow"
execute Drop = modifyStack $ \case
  (_ : xs) -> Right xs
  _ -> Left "Stack underflow"
execute Swap = modifyStack $ \case
  (x : y : xs) -> Right (y : x : xs)
  _ -> Left "Stack underflow"
execute Over = modifyStack $ \case
  (x : y : xs) -> Right (y : x : y : xs)
  _ -> Left "Stack underflow"
execute Rot = modifyStack $ \case
  (x : y : z : xs) -> Right (y : z : x : xs)
  _ -> Left "Stack underflow"
execute Dot = do
  stack' <- gets stack
  case stack' of
    (x : xs) -> do
      liftIO (print x)
      modifyStack (const (Right xs))
    _ -> throwError "Stack underflow"
execute Emit = do
  stack' <- gets stack
  case stack' of
    (x : xs) -> do
      liftIO (putChar (toEnum x))
      modifyStack (const (Right xs))
    _ -> throwError "Stack underflow"
execute CR = liftIO $ putStrLn ""
execute (DotString str) = liftIO $ putStr str
execute (If thenBranch elseBranch) = do
  stack' <- gets stack
  case stack' of
    (cond : xs) -> do
      modifyStack (const (Right xs))
      if cond /= 0
        then mapM_ execute thenBranch
        else mapM_ execute elseBranch
    _ -> throwError "Stack underflow"
execute (DoLoop body) = do
  stack' <- gets stack
  case stack' of
    (end : start : rest) -> do
      modifyStack (const (Right rest))
      forM_ [start .. end - 1] $ \i -> do
        modifyStack (Right . (i+1 :))
        mapM_ execute body
        modifyStack $ \s -> Right (tail s)
    _ -> throwError "DO requires two values on the stack"
execute I = do
  stack' <- gets stack
  case stack' of
    (i : _) -> modifyStack (Right . (i :))
    _ -> throwError "I requires a value on the stack"
execute (Variable name) = modify $ \s -> s { memory = Map.insert name 0 (memory s) }
execute (Set name) = do
  stack' <- gets stack
  case stack' of
    (value : xs) -> do
      modifyStack (const (Right xs))
      modify $ \s -> s { memory = Map.insert name value (memory s) }
    _ -> throwError "Stack underflow"
execute (Get name) = do
  mem <- gets memory
  case Map.lookup name mem of
    Just value -> modifyStack (Right . (value :))
    Nothing -> throwError $ "Variable " ++ name ++ " not found"
execute (Constant name value) = modify $ \s -> s { dictionary = Map.insert name [Push value] (dictionary s) }
execute (Word name wordDef) = modify $ \s -> s { dictionary = Map.insert name wordDef (dictionary s) }
execute (Call name) = do
  mem <- gets memory
  case Map.lookup name mem of
    Just wordDef -> do
      modifyStack (Right . (wordDef :))
    Nothing -> do
      throwError $ "Word " ++ name ++ " not found"
execute Comment = return ()
execute (NewArray n) = modifyStack $ \case
  stack -> Right (go n stack)
  where
    go 0 acc = acc
    go m acc = go (m-1) (0:acc)
execute (Index i) = modifyStack $ \case
  (arrIx : arr) -> Right (arr !! arrIx : arr)
  _ -> Left "Stack underflow"
execute (SetIndex i) = modifyStack $ \case
  (val : arrIx : arr) -> Right (take arrIx arr ++ [val] ++ drop (arrIx + 1) arr)
  _ -> Left "Stack underflow"
execute Begin = return ()
execute (Until cond) = do
  stack' <- gets stack
  case stack' of
    (x : xs) -> do
      if x == 0
        then return ()
        else mapM_ execute cond
    _ -> throwError "Stack underflow"

interpret :: String -> ColonM ()
interpret program = do
  let commands = parse program
  mapM_ execute commands

-- Parser for program
parseWord :: String -> [Command]
parseWord word
  | Just n <- readMaybe word = [Push n]
  | word == "+" = [Add]
  | word == "-" = [Sub]
  | word == "*" = [Mul]
  | word == "/" = [Div]
  | word == "MOD" = [Mod]
  | word == "DUP" = [Dup]
  | word == "DROP" = [Drop]
  | word == "SWAP" = [Swap]
  | word == "OVER" = [Over]
  | word == "ROT" = [Rot]
  | word == "=" = [Equals]
  | word == "<" = [Less]
  | word == ">" = [Greater]
  | word == "AND" = [And]
  | word == "OR" = [Or]
  | word == "NOT" = [Invert]
  | word == "DO" = [DoLoop []]
  | word == "LOOP" = []
  | word == "I" = [I]
  | word == "VARIABLE" = [Variable ""] -- Parse next word as variable name
  | word == "@" = [Get ""] -- Parse next word as variable name
  | word == "!" = [Set ""] -- Parse next word as variable name
  | word == "CONSTANT" = [Constant "" 0] -- Parse next word as constant name and number
  | word == "." = [Dot]
  | word == "EMIT" = [Emit]
  | word == "CR" = [CR]
  | "STRING:" `isPrefixOf` word = [DotString (drop 7 word)]
  | word == "\\" = [Comment]
  | word == "BEGIN" = [Begin]
  | word == "UNTIL" = [Until []]
  | otherwise = [Call word]

parse :: String -> [Command]
parse = fst . parseCommands . words
  where
    parseCommands :: [String] -> ([Command], [String])
    parseCommands [] = ([], [])
    parseCommands ("IF" : rest) =
      let (thenBranch, afterThen) = parseCommands rest
          (elseBranch, afterElse) = case dropWhile (/= "ELSE") afterThen of
            " ELSE " : r -> parseCommands r
            _ -> ([], afterThen)
          remaining = case dropWhile (/= "THEN ") afterElse of
            " THEN " : r -> r
            _ -> error "Unmatched IF/THEN"
       in (If thenBranch elseBranch : fst (parseCommands remaining), snd (parseCommands remaining))
    parseCommands ("ELSE" : rest) = parseCommands rest
    parseCommands ("THEN" : rest) = parseCommands rest
    parseCommands ("VARIABLE" : varName : rest) = (Variable varName : fst (parseCommands rest), snd (parseCommands rest))
    parseCommands ("@" : varName : rest) = (Get varName : fst (parseCommands rest), snd (parseCommands rest))
    parseCommands ("!" : varName : rest) = (Set varName : fst (parseCommands rest), snd (parseCommands rest))
    parseCommands ("CONSTANT" : constName : valueStr : rest) =
      case reads valueStr :: [(Int, String)] of
        [(value, "")] -> (Constant constName value : fst (parseCommands rest), snd (parseCommands rest))
        _ -> error "Invalid number for CONSTANT"
    parseCommands ("NEWARRAY" : sizeStr : rest) =
      case reads sizeStr :: [(Int, String)] of
        [(size, "")] -> (NewArray size : fst (parseCommands rest), snd (parseCommands rest))
        _ -> error "Invalid number for NEWARRAY"
    parseCommands ("INDEX" : idxStr : rest) =
      case reads idxStr :: [(Int, String)] of
        [(idx, "")] -> (Index idx : fst (parseCommands rest), snd (parseCommands rest))
        _ -> error "Invalid number for INDEX"
    parseCommands ("SETINDEX" : idxStr : numStr : rest) =
      case reads idxStr :: [(Int, String)] of
        [(idx, "")] -> case reads numStr :: [(Int, String)] of
          [(num, "")] -> (SetIndex idx : fst (parseCommands rest), snd (parseCommands rest))
          _ -> ([], rest)
        _ -> ([], rest)
    parseCommands ("BEGIN" : rest) =
      let (body, afterBody) = parseCommands rest
          (untilCmd, remaining) = parseCommands afterBody
      in case untilCmd of
           [Until _] -> (body ++ [Until body], remaining)
           _ -> error "Expected UNTIL command after loop body"
    parseCommands (word : rest) =
      let (cmds, remaining) = parseCommands rest
       in (parseWord word ++ cmds, remaining)

-- Run program
runColon :: String -> IO (Either String StateColon)
runColon input = do
  (result, finalState) <- runStateT (runExceptT (interpret input)) initState
  return (case result of
    Left err -> Left err
    Right () -> Right finalState)

initState :: StateColon
initState = StateColon [] Map.empty Map.empty

main :: IO ()
main = do
  let testCases = ["5 10 + .","1 2 3 + ","1 2 3 4 DUP","1 2 . . 3 . 4","VARIABLE x 42 x","5 0 >","5 0 > 1 ELSE 0 THEN .","3 4 + 5 * .","10 20 > 100 ELSE 200 THEN ."]
  forM_ testCases $ \testCase -> do
    putStrLn $ "Executing: " ++ testCase
    result <- runColon testCase
    case result of
      Left err -> putStrLn $ "Error: " ++ err
      Right st -> putStrLn $ "Stack: " ++ show (stack st)

{-
MORE test cases
      "5 10 + .",
      "1 2 3 + ",
      "1 2 3 4 DUP",
      "1 2 . . 3 . 4",
      "VARIABLE x 42 x",
      "5 0 >",
      "5 0 > 1 ELSE 0 THEN .",
      "3 4 + 5 * .",
      "10 20 > 100 ELSE 200 THEN .",
      "NEWARRAY 1 1 2 3 4 5 SETINDEX 0 INDEX .",
      "BEGIN 1 10 < UNTIL"-}
