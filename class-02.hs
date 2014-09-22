-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms n = (div n 3600, mod (div n 60) 60, mod n 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600 + m*60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

triangle :: Point -> Point -> Point -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
  where
    p = (distance (x1, y1) (x2, y2) + distance (x1, y1) (x3, y3) + distance (x3, y3) (x2, y2)) / 2
    s = sqrt (p * (p-distance (x1, y1) (x2, y2)) * (p-distance (x1, y1) (x3, y3)) * (p-distance (x3, y3) (x2, y2)))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = nEven xs + if mod x 2 == 0 then 1 else 0

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = 2*x : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if mod x 2 /= 0 then x : fltOdd xs else fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
delNegatives :: (Ord a, Num a) => [a] -> [a]
delNegatives [] = []
delNegatives (x:xs) = if x >= 0 then x : delNegatives xs else delNegatives xs

doubleEvens :: Integral a => [a] -> [a]
doubleEvens [] = []
doubleEvens (x:xs) = (if mod x 2 == 0 then 2*x else x) : doubleEvens xs

checkerboardOrder :: Num a => [a] -> [a]
checkerboardOrder [] = []
checkerboardOrder [x] = []
checkerboardOrder (x:y:xs) = y:x:checkerboardOrder xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y):combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_merge :: [a] -> [b] -> [(a,b)]
combine_merge [] ys = []
combine_merge xs [] = []
combine_merge (x:xs) (y:ys) = (x,y):combine_merge xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.
firstNatsDesc :: Integral n => n -> [n]
firstNatsDesc 0 = []
firstNatsDesc n = n:firstNatsDesc (n-1)

firstNatsAsc :: Integral n => n -> [n]
firstNatsAsc 0 = []
firstNatsAsc n = rightAppend (firstNatsAsc (n-1)) n
  where    
    rightAppend :: Integral n => [n] -> n -> [n]
    rightAppend [] n = [n]
    rightAppend (x:xs) n = x:(rightAppend xs n)

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
joinList :: a -> [a] -> [a]
joinList n [] = []
joinList n [x] = [x]
joinList n (x:xs) = x:n:(joinList n xs)

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).


copyFirst :: (Eq n, Num n) => [n] -> n -> [n]
copyFirst [] _ = []
copyFirst _ 0 = []
copyFirst [x] 1 = [x]
copyFirst (x:xs) n = x:(copyFirst xs (n-1))

copySince :: (Eq n, Num n) => [n] -> n -> [n]
copySince [] _ = []
copySince x 0 = x
copySince [x] _ = []
copySince (x:xs) n = copySince xs (n-1)

firstDiff :: (Eq n, Num n) => [n] -> n
firstDiff [] = 0
firstDiff [n] = 1
firstDiff (x:y:xs) = if x == y then 1 + firstDiff(y:xs) else 1
	
firstId :: (Eq n, Num n) => [n] -> ([n],[n])
firstId [] = ([],[])
firstId x = (copyFirst x (firstDiff x), copySince x (firstDiff x))

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- вернуть i-й элемент списка
find :: [a] -> Int -> a
find [] _ = undefined
find [x] 0 = x
find [x] _ = undefined
find (x:xs) 0 = x
find (x:xs) n = find xs (n-1)

-- б) Eq a => [a] -> a -> Bool
-- проверить, содержится ли элемент в списке
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains [x] n = x == n
contains (x:xs) n = x == n || contains xs n

-- в) [a] -> Int -> [a]
-- взять первые i элементов списка (copyFirst)
-- г) a -> Int -> [a]
-- составить список из i одинаковых элементов
multiply :: a -> Int -> [a]
multiply _ 0 = []
multiply x 1 = [x]
multiply x n = x:(multiply x (n-1))

-- д) [a] -> [a] -> [a]
-- combine_plus
-- е) Eq a => [a] -> [[a]]
-- сгруппировать элементы по признаку равенства в списки
group :: Eq a => [a] -> [[a]]
group [] = [[]]
group [x] = [[x]]
-- кажется, я переборщил. не вижу, как справиться без вспомогательной переменной

-- ж) [a] -> [(Int, a)]
-- перед каждым элементом вставить его индекс
lengthOf :: [a] -> Int
lengthOf [] = 0
lengthOf [x] = 1
lengthOf(x:xs) = 1 + length xs

indexing :: [a] -> [(Int, a)]
indexing x = combine_merge (firstNatsAsc (lengthOf x)) x

-- з) Eq a => [a] -> [a]
-- выбросить из списка все повторные элементы
-- тут тоже как-то круто...