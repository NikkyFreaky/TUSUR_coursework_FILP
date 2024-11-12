import Control.Monad (zipWithM_)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Map (Map, elems, fromList, (!?))
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import System.Exit (exitSuccess)
import System.IO
import Text.Read (readMaybe)

-- Определение типов данных
data VehiclePart = VehiclePart
  { partName :: String,
    partPrice :: Double,
    partType :: String,
    partBrand :: String,
    partOriginal :: Bool
  }
  deriving (Show, Read, Eq, Ord)

data Answer = Answer
  { answer :: String,
    qid :: Int
  }
  deriving (Show, Read)

data Question = Question
  { question :: String,
    questionId :: Int,
    answers :: [Answer]
  }
  deriving (Show, Read)

data Problem = Problem
  { problemDescription :: String,
    startingQuestionId :: Int
  }

data Cart = Cart [VehiclePart] Double (Set.Set VehiclePart) deriving (Show)

emptyCart :: Cart
emptyCart = Cart [] 0.0 Set.empty

-- Добавление в корзину с учетом уникальности частей
addToCart :: Cart -> VehiclePart -> Cart
addToCart (Cart items total set) part =
  if part `Set.member` set
    then Cart items total set
    else Cart (part : items) (total + partPrice part) (Set.insert part set)

-- Удаление из корзины
removeFromCart :: Cart -> Int -> Cart
removeFromCart (Cart items total set) index
  | index < 1 || index > length items = Cart items total set
  | otherwise =
      let (keep, remove) = splitAt (index - 1) items
          newItems = keep ++ drop 1 remove
          removedPart = head remove
          newTotal = total - partPrice removedPart
          newSet = Set.delete removedPart set
       in Cart newItems newTotal newSet

-- Универсальная функция для ввода с проверкой
getValidInput :: (Read a) => String -> (a -> Bool) -> IO a
getValidInput prompt check = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just value | check value -> return value
    _ -> do
      putStrLn "Неверный ввод. Попробуйте еще раз."
      getValidInput prompt check

-- Чтение файла с автозапчастями
readVehicleParts :: FilePath -> IO [VehiclePart]
readVehicleParts filePath = do
  handle <- openFile filePath ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  let parts = parseVehicleParts contents
  length parts `seq` hClose handle -- Гарантируем чтение перед закрытием
  return parts

parseVehicleParts :: String -> [VehiclePart]
parseVehicleParts = mapMaybe parseVehiclePartLine . lines

parseVehiclePartLine :: String -> Maybe VehiclePart
parseVehiclePartLine = readMaybe

-- Чтение файла с вопросами
readQuestions :: FilePath -> IO [Question]
readQuestions filePath = do
  handle <- openFile filePath ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  let questions = parseQuestions contents
  length questions `seq` hClose handle -- Гарантируем чтение перед закрытием
  return questions

parseQuestions :: String -> [Question]
parseQuestions = mapMaybe parseQuestionLine . lines

parseQuestionLine :: String -> Maybe Question
parseQuestionLine = readMaybe

-- Получение уникальных типов и брендов
getUniqueParts :: [VehiclePart] -> [String]
getUniqueParts = nub . map partType

getUniqueBrands :: [VehiclePart] -> [String]
getUniqueBrands = nub . map partBrand

-- Функция для фильтрации автозапчастей
filterVehicleParts :: String -> String -> (Double, Double) -> Bool -> [VehiclePart] -> [VehiclePart]
filterVehicleParts typ brand (minPrice, maxPrice) isOriginal =
  filter ((== typ) . partType)
    . filter ((== brand) . partBrand)
    . filter ((>= minPrice) . partPrice)
    . filter ((<= maxPrice) . partPrice)
    . filter ((== isOriginal) . partOriginal)

-- Список проблем для диагностики
problems :: [Problem]
problems =
  [ Problem "Нестандартная работа двигателя" 1,
    Problem "Автомобиль теряет мощность" 2,
    Problem "Автомобиль плохо заводится" 3,
    Problem "Повышенный расход топлива" 4,
    Problem "Необычные звуки при работе двигателя" 5
  ]

-- Основное меню
main :: IO ()
main = do
  putStrLn "Выберите опцию:"
  putStrLn "1) Купить запчасти"
  putStrLn "2) Помощь в подборе запчастей"
  putStrLn "3) Выйти\n"
  option <- getLine
  case option of
    "1" -> buyVehicleParts emptyCart
    "2" -> do
      questions <- readQuestions "questions.txt"
      helpSelectVehicleParts questions
    "3" -> exitSuccess
    _ -> putStrLn "Неверный выбор\n" >> main

-- Покупка автозапчастей
buyVehicleParts :: Cart -> IO ()
buyVehicleParts cart = do
  vehicleParts <- readVehicleParts "vehicle_parts.txt"
  let uniqueParts = getUniqueParts vehicleParts
  putStrLn "\nВыберите тип запчасти:"
  zipWithM_ (\i a -> putStrLn $ show (i :: Int) ++ ") " ++ a) [1 ..] uniqueParts
  putStrLn "0) Выйти в главное меню"
  answer <- getValidInput "" (\x -> x >= 0 && x <= length uniqueParts)
  if answer == 0
    then main
    else do
      let typ = uniqueParts !! (answer - 1)
      putStrLn "\nВыберите марку автомобиля:"
      let uniqueBrands = getUniqueBrands vehicleParts
      zipWithM_ (\i a -> putStrLn $ show (i :: Int) ++ ") " ++ a) [1 ..] uniqueBrands
      answer <- getValidInput "" (\x -> x >= 0 && x <= length uniqueBrands)
      if answer == 0
        then main
        else do
          let brand = uniqueBrands !! (answer - 1)
          minPrice <- getValidInput "\nВведите минимальную цену (0 для любой):" (>= 0)
          maxPrice <- getValidInput "Введите максимальную цену (0 для любой):" (>= minPrice)
          isOriginal <- getValidInput "\nВам нужна оригинальная запчасть? (1 - Да, 2 - Нет):" (\x -> x == 1 || x == 2)
          let original = isOriginal == 1
          buyVehiclePartsWithFilter typ brand (minPrice, maxPrice) original vehicleParts cart

-- Покупка автозапчастей с фильтрами
buyVehiclePartsWithFilter :: String -> String -> (Double, Double) -> Bool -> [VehiclePart] -> Cart -> IO ()
buyVehiclePartsWithFilter typ brand (minPrice, maxPrice) isOriginal vehicleParts cart = do
  let filteredByType = filter ((== typ) . partType) vehicleParts
  let filteredByBrand = filter ((== brand) . partBrand) filteredByType
  let filteredByPrice = filter (\p -> (minPrice == 0 || partPrice p >= minPrice) && (maxPrice == 0 || partPrice p <= maxPrice)) filteredByBrand
  let filtered = filter ((== isOriginal) . partOriginal) filteredByPrice
  if null filtered
    then putStrLn "К сожалению, по вашему запросу не было найдено запчастей." >> buyVehiclePartsCart cart
    else do
      putStrLn "\nДоступные запчасти:"
      zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ partName a ++ " | " ++ show (partPrice a) ++ " | " ++ partType a ++ " | " ++ partBrand a ++ if partOriginal a then " Оригинальная" else " Не оригинальная") [1 ..] filtered
      putStrLn "Введите номера запчастей для добавления в корзину (через пробел) или нажмите Enter для продолжения:"
      input <- getLine
      buyVehiclePartsCart $ processInput cart (words input) filtered

-- Обработка ввода для корзины
processInput :: Cart -> [String] -> [VehiclePart] -> Cart
processInput cart [] _ = cart
processInput cart (x : xs) parts =
  case readMaybe x of
    Just index | index > 0 && index <= length parts -> processInput (addToCart cart (parts !! (index - 1))) xs parts
    _ -> processInput cart xs parts

-- Вывод содержимого корзины
buyVehiclePartsCart :: Cart -> IO ()
buyVehiclePartsCart cart@(Cart items total _) = do
  putStrLn "\nКорзина:"
  zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ partName a ++ " | " ++ show (partPrice a) ++ " | " ++ partType a ++ " | " ++ partBrand a ++ if partOriginal a then " Оригинальная" else " Не оригинальная") [1 .. length items] items
  putStrLn $ "Сумма покупки: " ++ show total
  choice <- getValidInput "Выберите действие (1 - Завершить покупку, 2 - Продолжить покупки, 3 - Удалить из корзины, 0 - Главное меню):" (\x -> x >= 0 && x <= 3)
  case choice of
    1 -> do
      putStrLn "\nПокупка совершена!"
      putStrLn "\nНажмите Enter для выхода"
      _ <- getLine
      exitSuccess
    2 -> buyVehicleParts cart
    3 -> do
      index <- getValidInput "Введите номер запчасти для удаления:" (\x -> x > 0 && x <= length items)
      buyVehiclePartsCart $ removeFromCart cart index
    0 -> main
    _ -> buyVehiclePartsCart cart

-- Диагностика по вопросам
helpSelectVehicleParts :: [Question] -> IO ()
helpSelectVehicleParts questions = do
  putStrLn "\nВыберите проблему:"
  zipWithM_ (\i p -> putStrLn $ show i ++ ") " ++ problemDescription p) [1 ..] problems
  choice <- getValidInput "0) Выйти в главное меню\n" (\x -> x >= 0 && x <= length problems)
  if choice == 0
    then main
    else do
      let selectedProblem = problems !! (choice - 1)
      helpSelectVehiclePartsRec (fromList [(questionId q, q) | q <- questions]) (startingQuestionId selectedProblem)

-- Рекурсивная диагностика по вопросам
helpSelectVehiclePartsRec :: Map Int Question -> Int -> IO ()
helpSelectVehiclePartsRec questionsMap currentId = do
  case questionsMap !? currentId of
    Nothing -> putStrLn "Неизвестная проблема" >> helpSelectVehicleParts (elems questionsMap)
    Just q -> do
      putStrLn (question q)
      if null (answers q)
        then putStrLn "\nЗавершение диагностики." >> main
        else do
          zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ answer a) [1 ..] (answers q)
          answerIndex <- getValidInput "\nВыберите ответ:" (\x -> x >= 0 && x <= length (answers q))
          if answerIndex == 0
            then main
            else helpSelectVehiclePartsRec questionsMap (qid $ answers q !! (answerIndex - 1))