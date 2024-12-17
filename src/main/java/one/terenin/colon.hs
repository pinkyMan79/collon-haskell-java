import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Text.Read (readMaybe)

type Stack = [Int]
type WordDef = [Command]

data Command
  = Push Int                  -- Кладет число в стек
  | Add | Sub | Mul | Div | Mod -- Арифметика
  | Dup | Drop | Swap | Over | Rot -- Манипуляция стеком
  | Emit | CR | Dot | DotString String -- Ввод-вывод
  | Equals | Less | Greater | And | Or | Invert -- Логика
  | If [Command] [Command]   -- Условный оператор
  | Do [Command]             -- Цикл
  | Variable String          -- Объявление переменной
  | Set String               -- Установка значения переменной
  | Get String               -- Получение значения переменной
  | Constant String Int      -- Константа
  | Word String WordDef      -- Новое слово
  | Comment                  -- Игнорируем комментарий
  deriving (Show, Eq)

type Dictionary = Map String Command

data StateColon = StateColon
  { stack      :: Stack
  , dictionary :: Dictionary
  , memory     :: Map String Int -- Хранилище для переменных
  }

type ColonM = ExceptT String (State StateColon)

-- Главная функция выполнения программы
interpret :: String -> ColonM ()
interpret program = mapM_ execute (parse program)

-- Выполнение команды
execute :: Command -> ColonM ()
execute = undefined -- Реализуем позже

-- Парсер программы
parse :: String -> [Command]
parse = undefined -- Реализуем позже

execute (Push n) = modifyStack (n :)
execute Add = binaryOp (+)
execute Sub = binaryOp (-)
execute Mul = binaryOp (*)
execute Div = binaryOp div
execute Mod = binaryOp mod

binaryOp :: (Int -> Int -> Int) -> ColonM ()
binaryOp op = modifyStack $ \case
  (x:y:rest) -> op y x : rest
  _ -> throwError "Stack underflow"

modifyStack :: (Stack -> Stack) -> ColonM ()
modifyStack f = modify $ \st -> st { stack = f (stack st) }

execute Dup = modifyStack $ \case
  (x:xs) -> x : x : xs
  _ -> throwError "Stack underflow"

execute Drop = modifyStack $ \case
  (_:xs) -> xs
  _ -> throwError "Stack underflow"

execute Swap = modifyStack $ \case
  (x:y:xs) -> y : x : xs
  _ -> throwError "Stack underflow"

execute Over = modifyStack $ \case
  (x:y:xs) -> y : x : y : xs
  _ -> throwError "Stack underflow"

execute Rot = modifyStack $ \case
  (x:y:z:xs) -> y : z : x : xs
  _ -> throwError "Stack underflow"

execute Dot = modifyStack $ \case
  (x:xs) -> unsafePerformIO (print x) `seq` xs
  _ -> throwError "Stack underflow"

execute Emit = modifyStack $ \case
  (x:xs) -> unsafePerformIO (putChar (toEnum x)) `seq` xs
  _ -> throwError "Stack underflow"

execute CR = liftIO $ putStrLn ""

execute (DotString str) = liftIO $ putStr str

execute (If thenBranch elseBranch) = do
  stack' <- gets stack
  case stack' of
    (x:xs) -> do
      modifyStack (const xs)
      if x /= 0
        then mapM_ execute thenBranch
        else mapM_ execute elseBranch
    _ -> throwError "Stack underflow"

parse :: String -> [Command]
parse input = concatMap parseWord (words input)

parseWord :: String -> [Command]
parseWord word
  -- Число
  | Just n <- readMaybe word = [Push n]

  -- Арифметика
  | word == "+"   = [Add]
  | word == "-"   = [Sub]
  | word == "*"   = [Mul]
  | word == "/"   = [Div]
  | word == "MOD" = [Mod]

  -- Манипуляция стеком
  | word == "DUP"   = [Dup]
  | word == "DROP"  = [Drop]
  | word == "SWAP"  = [Swap]
  | word == "OVER"  = [Over]
  | word == "ROT"   = [Rot]

  -- Логика
  | word == "="     = [Equals]
  | word == "<"     = [Less]
  | word == ">"     = [Greater]
  | word == "AND"   = [And]
  | word == "OR"    = [Or]
  | word == "NOT"   = [Invert]

  -- Условные операторы
  | word == "IF"    = [If [] []]
  | word == "DO"    = [Do []]

  -- Работа с переменными
  | word == "VARIABLE" = [Variable ""]
  | word == "@"        = [Get]
  | word == "!"        = [Set]
  | word == "CONSTANT" = [Constant "" 0]

  -- Ввод-вывод
  | word == "."         = [Dot]
  | word == "EMIT"      = [Emit]
  | word == "CR"        = [CR]
  | "STRING:" `isPrefixOf` word = [DotString (drop 7 word)] -- Вывод строки после "STRING:"

  -- Комментарии
  | word == "\\" = [Comment]

  -- Определение слова
  | otherwise = [Word word []]


main :: IO ()
main = do
  putStrLn "Введите программу Colon:"
  input <- getLine
  case runColon input of
    Left err -> putStrLn $ "Ошибка: " ++ err
    Right st -> putStrLn $ "Стек: " ++ show (stack st)


-- tests

testPrograms :: [String]
testPrograms =
  [ "5 10 + ."                     -- Тест арифметики
  , "5 0 > IF 1 ELSE 0 THEN ."     -- Тест условия
  , "1 5 DO I . LOOP"              -- Тест цикла
  , "VARIABLE x 42 x ! x @ ."      -- Тест переменных
  , "3 4 + 5 * ."                  -- Сложное выражение
  , "10 20 > IF 100 ELSE 200 THEN ." -- Тест сравнения
  ]

runTest :: String -> IO ()
runTest prog =
  case runColon prog of
    Left err -> putStrLn $ "Ошибка: " ++ err
    Right st -> putStrLn $ "Стек: " ++ show (stack st)

main :: IO ()
main = mapM_ runTest testPrograms
