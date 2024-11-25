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
  putStr prompt
  hFlush stdout
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

-- Функция для фильтрации запчастей с учётом совместимых брендов
filterVehicleParts :: String -> String -> (Double, Double) -> Bool -> [VehiclePart] -> [VehiclePart]
filterVehicleParts typ brand (minPrice, maxPrice) isOriginal vehicleParts =
  let compatibleBrands = getCompatibleBrands typ brand
   in filter ((== typ) . partType)
        . filter (\p -> partBrand p `elem` compatibleBrands)
        . filter (\p -> minPrice == 0 || partPrice p >= minPrice)
        . filter (\p -> maxPrice == 0 || partPrice p <= maxPrice)
        . filter (\p -> partOriginal p == isOriginal)
        $ vehicleParts

-- Функция для получения списка совместимых брендов
getCompatibleBrands :: String -> String -> [String]
getCompatibleBrands typ brand
  | typ == "Воздушный фильтр" && brand `elem` ["Audi", "Volkswagen"] =
      ["Audi", "Volkswagen"]
  | typ == "Топливный фильтр" && brand `elem` ["BMW", "Mercedes-Benz"] =
      ["BMW", "Mercedes-Benz"]
  | typ == "Генератор" && brand `elem` ["BMW", "Mercedes-Benz"] =
      ["BMW", "Mercedes-Benz"]
  | typ == "Свеча зажигания" && brand `elem` ["Audi", "Volkswagen", "Ford", "BMW", "Mercedes-Benz"] =
      ["Audi", "Volkswagen", "Ford", "BMW", "Mercedes-Benz"]
  | typ == "Свеча зажигания" && brand `elem` ["KIA", "Toyota", "Lada"] =
      ["KIA", "Toyota", "Lada"]
  | otherwise = [brand]

-- Список проблем для диагностики
problems :: [Problem]
problems =
  [ Problem "Проблемы с запуском двигателя" 1,
    Problem "Автомобиль теряет мощность" 6,
    Problem "Автомобиль плохо заводится" 9,
    Problem "Повышенный расход топлива" 12
  ]

-- Функция для отображения меню и вывода с настраиваемым приглашением
promptAction :: String -> String -> IO String
promptAction menu promptText = do
  putStrLn menu
  putStr promptText
  hFlush stdout
  getLine

-- Функция для выбора марки автомобиля
selectBrand :: String -> [VehiclePart] -> Cart -> IO ()
selectBrand typ vehicleParts cart = do
  let uniqueBrands = getUniqueBrands vehicleParts
  answer <- promptAction ("\n" ++ unlines (zipWith (\i brand -> show i ++ ") " ++ brand) [1 ..] uniqueBrands ++ ["0) Выйти в главное меню"])) "Выберите действие: "
  if answer == "0"
    then mainWithCart cart "Выберите действие: "
    else do
      let brand = uniqueBrands !! (read answer - 1)
      filterParts typ brand vehicleParts cart

-- Функция для фильтрации и покупки в меню
filterParts :: String -> String -> [VehiclePart] -> Cart -> IO ()
filterParts typ brand vehicleParts cart = do
  minPrice <- getValidInput "\nВведите минимальную цену (0 для любой): " (>= 0)
  maxPrice <- getValidInput "Введите максимальную цену (0 для любой): " (>= minPrice)
  isOriginal <- getValidInput "Вам нужна оригинальная запчасть? (1 - Да, 2 - Нет): " (\x -> x == 1 || x == 2)
  let original = isOriginal == 1
  buyVehiclePartsWithFilter typ brand (minPrice, maxPrice) original vehicleParts cart

-- Функция просмотра корзины
viewCart :: Cart -> IO ()
viewCart cart@(Cart items total _) = do
  putStrLn "\nСодержимое корзины:"
  if null items
    then putStrLn "Корзина пуста."
    else do
      zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ partName a ++ " | " ++ show (partPrice a) ++ " | " ++ partType a ++ " | " ++ partBrand a ++ if partOriginal a then " Оригинальная" else " Не оригинальная") [1 ..] items
      putStrLn $ "\nОбщая сумма: " ++ show total
  putStrLn "\nНажмите Enter, чтобы вернуться в главное меню."
  _ <- getLine
  mainWithCart cart "Выберите действие: "

-- Основное меню
main :: IO ()
main = mainWithCart emptyCart "Выберите действие: "

mainWithCart :: Cart -> String -> IO ()
mainWithCart cart promptText = do
  option <- promptAction "\n1) Купить запчасти\n2) Помощь в подборе запчастей\n3) Просмотреть корзину\n0) Выйти\n" promptText
  case option of
    "1" -> buyVehicleParts cart
    "2" -> do
      questions <- readQuestions "questions.txt"
      helpSelectVehicleParts questions cart
    "3" -> viewCart cart
    "0" -> exitSuccess
    _ -> putStrLn "Неверный выбор\n" >> mainWithCart cart promptText

-- Покупка автозапчастей
buyVehicleParts :: Cart -> IO ()
buyVehicleParts cart = do
  vehicleParts <- readVehicleParts "vehicle_parts.txt"
  let uniqueParts = getUniqueParts vehicleParts
  answer <- promptAction ("\n" ++ unlines (zipWith (\i part -> show i ++ ") " ++ part) [1 ..] uniqueParts ++ ["0) Выйти в главное меню"])) "Выберите действие: "
  if answer == "0"
    then mainWithCart cart "Выберите действие: "
    else do
      let typ = uniqueParts !! (read answer - 1)
      selectBrand typ vehicleParts cart

-- Покупка автозапчастей с фильтрами
buyVehiclePartsWithFilter :: String -> String -> (Double, Double) -> Bool -> [VehiclePart] -> Cart -> IO ()
buyVehiclePartsWithFilter typ brand (minPrice, maxPrice) isOriginal vehicleParts cart = do
  let filtered = filterVehicleParts typ brand (minPrice, maxPrice) isOriginal vehicleParts
  if null filtered
    then putStrLn "\nК сожалению, по вашему запросу не было найдено запчастей." >> buyVehiclePartsCart cart
    else do
      putStrLn "\nДоступные запчасти:"
      zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ partName a ++ " | " ++ show (partPrice a) ++ " | " ++ partType a ++ " | " ++ partBrand a ++ if partOriginal a then " Оригинальная" else " Не оригинальная") [1 ..] filtered
      putStrLn "\nВведите номера запчастей для добавления в корзину (через пробел) или нажмите Enter для продолжения:"
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
  if null items
    then putStrLn "Корзина пуста."
    else do
      zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ partName a ++ " | " ++ show (partPrice a) ++ " | " ++ partType a ++ " | " ++ partBrand a ++ if partOriginal a then " Оригинальная" else " Не оригинальная") [1 .. length items] items
      putStrLn $ "\nСумма покупки: " ++ show total
  putStrLn "\nЧто вы хотите сделать?\n1) Завершить покупку\n2) Продолжить покупки\n3) Удалить из корзины\n0) Выйти в главное меню"
  choice <- promptAction "" "\nВыберите действие: "
  case choice of
    "1" -> do
      putStrLn "\nПокупка совершена!"
      putStrLn "\nНажмите Enter для выхода"
      _ <- getLine
      exitSuccess
    "2" -> buyVehicleParts cart
    "3" -> do
      if null items
        then putStrLn "Корзина пуста. Удаление невозможно." >> buyVehiclePartsCart cart
        else do
          index <- getValidInput "\nВведите номер запчасти для удаления:" (\x -> x > 0 && x <= length items)
          buyVehiclePartsCart $ removeFromCart cart index
    "0" -> mainWithCart cart "Выберите действие: "
    _ -> putStrLn "Неверный выбор. Попробуйте снова." >> buyVehiclePartsCart cart

-- Диагностика по вопросам
helpSelectVehicleParts :: [Question] -> Cart -> IO ()
helpSelectVehicleParts questions cart = do
  let menu =
        "\nВыберите проблему:\n"
          ++ unlines (zipWith (\i p -> show i ++ ") " ++ problemDescription p) [1 ..] problems)
          ++ "0) Выйти в главное меню\n"
  choiceStr <- promptAction menu "Выберите действие: "
  putStrLn ""
  case readMaybe choiceStr of
    Just choice | choice >= 0 && choice <= length problems -> do
      if choice == 0
        then mainWithCart cart "Выберите действие: "
        else do
          let selectedProblem = problems !! (choice - 1)
          helpSelectVehiclePartsRec (fromList [(questionId q, q) | q <- questions]) (startingQuestionId selectedProblem) cart
    _ -> do
      putStrLn "Неверный выбор. Попробуйте снова."
      helpSelectVehicleParts questions cart

-- Рекурсивная диагностика по вопросам
helpSelectVehiclePartsRec :: Map Int Question -> Int -> Cart -> IO ()
helpSelectVehiclePartsRec questionsMap currentId cart = do
  case questionsMap !? currentId of
    Nothing -> putStrLn "Неизвестная проблема" >> helpSelectVehicleParts (elems questionsMap) cart
    Just q -> do
      putStrLn (question q)
      if null (answers q)
        then do
          putStrLn "\nЗавершение диагностики."
          mainWithCart cart "Выберите дальнейшее действие: "
        else do
          zipWithM_ (\i a -> putStrLn $ show i ++ ") " ++ answer a) [1 ..] (answers q)
          answerIndex <- getValidInput "\nВыберите ответ: " (\x -> x >= 0 && x <= length (answers q))
          putStrLn ""
          if answerIndex == 0
            then mainWithCart cart "Выберите действие: "
            else helpSelectVehiclePartsRec questionsMap (qid $ answers q !! (answerIndex - 1)) cart
