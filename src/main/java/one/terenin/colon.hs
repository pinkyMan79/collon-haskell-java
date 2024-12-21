{-# LANGUAGE LambdaCase #-}

import Data.Map (Map)
import Data.Bits ( (.&.), (.|.) )
import Control.Monad ( forM_ )
import Data.List ( isPrefixOf )
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafePerformIO)

-- Стек для хранения чисел
type Stack = [Int]

-- Определение нового слова
type WordDef = [Command]

-- Тип данных, представляющий команду языка Colon
data Command
  = Push Int                    -- Кладет число в стек
  | Add | Sub | Mul | Div | Mod -- Арифметические операции
  | Dup | Drop | Swap | Over | Rot -- Манипуляция стеком
  | Emit | CR | Dot | DotString String -- Ввод-вывод
  | Equals | Less | Greater | And | Or | Invert -- Логические операции
  | If [Command] [Command]     -- Условный оператор IF
  | DoLoop [Command]           -- Цикл DO ... LOOP
  | I                          -- Получить текущий индекс цикла
  | Variable String            -- Объявление переменной
  | Set String                 -- Установка значения переменной
  | Get String                 -- Получение значения переменной
  | Constant String Int        -- Объявление константы
  | Word String WordDef        -- Определение нового слова
  | Comment                    -- Комментарий, игнорируем команду
  deriving (Show, Eq)

-- Словарь для хранения определённых слов
type Dictionary = Map String Command

-- Состояние программы Colon
data StateColon = StateColon
  { stack      :: Stack           -- Стек для чисел
  , dictionary :: Dictionary      -- Словарь слов
  , memory     :: Map String Int  -- Память для переменных
  }

-- Монада для обработки состояния и ошибок
type ColonM = ExceptT String (StateT StateColon IO)

-- Главная функция интерпретации программы
interpret :: String -> ColonM ()
interpret program = mapM_ execute (parse program)

-- Выполнение команды
execute :: Command -> ColonM ()
execute (Push n) = modifyStack (n :)

-- Арифметические операции
execute Add = binaryOp (+)
execute Sub = binaryOp (-)
execute Mul = binaryOp (*)
execute Div = binaryOp div
execute Mod = binaryOp mod

-- Обработка логических операций
execute Equals = binaryOp (\x y -> if x == y then 1 else 0)
execute Less   = binaryOp (\x y -> if x < y then 1 else 0)
execute Greater = binaryOp (\x y -> if x > y then 1 else 0)
execute And    = binaryOp (.&.)
execute Or     = binaryOp (.|.)
execute Invert = modifyStack $ \case
  (x : xs) -> (if x == 0 then 1 else 0) : xs
  _ -> undefined

-- Манипуляции со стеком
execute Dup = modifyStack $ \case
  (x:xs) -> x : x : xs
  _ -> undefined

execute Drop = modifyStack $ \case
  (_:xs) -> xs
  _ -> undefined

execute Swap = modifyStack $ \case
  (x:y:xs) -> y : x : xs
  _ -> undefined

execute Over = modifyStack $ \case
  (x:y:xs) -> y : x : y : xs
  _ -> undefined

execute Rot = modifyStack $ \case
  (x:y:z:xs) -> y : z : x : xs
  _ -> undefined

-- Ввод-вывод
execute Dot = modifyStack $ \case
  (x:xs) -> unsafePerformIO (print x) `seq` xs
  _ -> undefined

execute Emit = modifyStack $ \case
  (x:xs) -> unsafePerformIO (putChar (toEnum x)) `seq` xs
  _ -> undefined

execute CR = liftIO $ putStrLn ""

execute (DotString str) = liftIO $ putStr str

-- Условный оператор IF ... ELSE
execute (If thenBranch elseBranch) = do
  stack' <- gets stack
  case stack' of
    (x:xs) -> do
      modifyStack (const xs)
      if x /= 0
        then mapM_ execute thenBranch
        else mapM_ execute elseBranch
    _ -> undefined

-- Обработка цикла DO ... LOOP
execute (DoLoop body) = do
  stack' <- gets stack
  case stack' of
    (end:start:rest) -> do
      -- Убираем границы из стека
      modifyStack (const rest)
      -- Выполняем цикл от start до end (не включая end)
      forM_ [start..(end-1)] $ \i -> do
        modifyStack (i :)  -- Добавляем индекс цикла
        mapM_ execute body -- Выполняем тело цикла
        modifyStack tail   -- Удаляем индекс после итерации
    _ -> throwError "DO requires two values on the stack"

-- Получить текущий индекс цикла
execute I = modifyStack $ \case
  xs@(i:_) -> i : xs
  _ -> undefined

-- Универсальная функция для выполнения бинарных операций
binaryOp :: (Int -> Int -> Int) -> ColonM ()
binaryOp op = modifyStack $ \case
  (x:y:rest) -> op y x : rest
  _ -> undefined

-- Изменение состояния стека
modifyStack :: (Stack -> Stack) -> ColonM ()
modifyStack f = do
  stack <- gets stack
  let newStack = f stack
  modify $ \s -> s { stack = newStack }

-- Парсер программы: преобразует строку в список команд
parse :: String -> [Command]
parse input = concatMap parseWord (words input)

parseWord :: String -> [Command]
parseWord word
  | Just n <- readMaybe word = [Push n]
  | word == "+"   = [Add]
  | word == "-"   = [Sub]
  | word == "*"   = [Mul]
  | word == "/"   = [Div]
  | word == "MOD" = [Mod]
  | word == "DUP"  = [Dup]
  | word == "DROP" = [Drop]
  | word == "SWAP" = [Swap]
  | word == "OVER" = [Over]
  | word == "ROT"  = [Rot]
  | word == "="     = [Equals]
  | word == "<"     = [Less]
  | word == ">"     = [Greater]
  | word == "AND"   = [And]
  | word == "OR"    = [Or]
  | word == "NOT"   = [Invert]
  | word == "DO"    = [DoLoop []]
  | word == "LOOP"  = []
  | word == "I"     = [I]
  | word == "VARIABLE" = [Variable ""]
  | word == "@"        = [Get ""]
  | word == "!"        = [Set ""]
  | word == "CONSTANT" = [Constant "" 0]
  | word == "."         = [Dot]
  | word == "EMIT"      = [Emit]
  | word == "CR"        = [CR]
  | "STRING:" `isPrefixOf` word = [DotString (drop 7 word)]
  | word == "\\" = [Comment]
  | otherwise = [Word word []]

-- Запуск программы
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
  putStrLn "Введите программу:"
  prog <- getLine
  result <- runColon prog
  case result of
    Left err -> putStrLn $ "Ошибка: " ++ err
    Right st -> putStrLn $ "Стек: " ++ show (stack st)



testPrograms :: [String]
testPrograms =
  [ "5 10 + ."                     -- Тест сложения
  , "5 0 > IF 1 ELSE 0 THEN ."     -- Тест условия
  , "1 5 DO I . LOOP"              -- Тест цикла
  , "VARIABLE x 42 x ! x @ ."      -- Тест переменных
  , "3 4 + 5 * ."                  -- Сложное арифметическое выражение
  , "10 20 > IF 100 ELSE 200 THEN ." -- Тест сравнения
  ]

-- Функция запуска теста
--runTest :: String -> IO ()
--runTest prog =
--  case runColon prog of
--    Left err -> putStrLn $ "Ошибка: " ++ err
--    Right st -> putStrLn $ "Стек: " ++ show (stack st)