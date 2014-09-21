-- IT314. Функциональное программирование
-- Занятие 1

-- 1) Функция без параметров (= константа)

hello :: String -- типовая аннотация (сигнатура)
hello = "Hello, world"

{-
  Запустите интерпретатор ghci (из каталога, в котором находится этот файл),
  загрузите этот файл:

> :load class-01

  и вызовите функцию hello:

> hello

-}

-- 2) Объявление функций

-- тип функции: Два параметра типа Double и результат того же типа
avg :: Double -> Double -> Double
avg a b = (a + b)/2

{-

  Пример вызова (передача параметров через пробел,
  пробел -- это операция вызова функции (применение)!):

> avg 5 9
7.0

  Функция может вызываться инфиксно:

> 5 `avg` 9
7.0

  а) Вычислите в ghci среднее арифметическое следующих пар чисел: 332 и 723, 34.34 и 93.27.
     Впишите ответы: 527.5, 63.805

  б) Напишите функцию avg3, вычисляющую среднее арифметическое трёх заданных чисел.
     Проверьте результаты её работы на двух тройках чисел.

-}

avg3 :: Double -> Double -> Double -> Double
avg3 a b c = (a + b + c)/3

{-
   После определения функции avg3 этот файл следует перезагрузить. Для этого в ghci необходимо выполнить
   команду :reload (или :r).

   Результаты проверки:

> avg3 1 2 3
2.0

> avg3 13.1256 245.456 543.3
267.29386666666664

-}

-- 3) Выражения

{-
   Вычислите и сохраните в этом файле значения следующих выражений,
   обращая внимание на обозначения и приоритеты операций, стандартные функции,
   расстановку скобок:

    2 + 3 = 5
    mod 10 4 = 2
    10 `mod` 4 = 2 (NB: '' не работают)
    True && 5 < 10 = True
    5 < 7 || 10 > 3 = True
    sqrt (-2) = NaN
    sqrt (sqrt 16) = 2.0
    let x = 4 in (sin x)^2 + (cos x)^2 = 1.0
    x = <interactive>:20:1: Not in scope: `x'
    7^(-1) = *** Exception: Negative exponent
    error "AAAA!!!!"= *** Exception: AAAA!!!!
    12345^54321 (ок, посчитал)
    2 < 3 || 9999954321^99912345 > 12345^54321 = True

-}

-- 4) Типы

{-
  Тип выражения можно узнать, воспользовавшись командой интерпретатора :t, например:

> :t 'a'
'a' :: Char
> :t 1
1 :: Num a => a

  Запись "1 :: Num a => a" означает, что выражение "1" имеет тип "a", где "a" принадлежит
  классу типов Num (имеет экземпляр класса типов Num, является числовым типом).

  Определите и сохраните в этом файле типы следующих выражений:
   5 :: Num a => a
   5.0 :: Fractional a => a
   sqrt 4 :: Floating a => a
   sqrt 4.0 :: Floating a => a
   2+3 :: Num a => a
   5 < 7 :: Bool
   if 2 > 3 then 7 else 5 :: Num a => a
   5 > 6 && False :: Bool

   Команда ":set +t" включает режим, при котором печатается тип каждого вычисляемого выражения.
   Команда ":set +s" включает режим, при котором печатается время вычисления каждого выражения.

-}

-- 5) Объявление функций (2)

-- а) Удвоение значения заданного числа
<<<<<<< HEAD
-- (объясните смысл типовой аннотации: аргумент и результат функции принадлежат к типу a из класса типов Num)
=======
-- (типовая аннотация здесь означает, что функция принимает один параметр типа a и возвращает значение
--  типа a, причём тип a принадлежит классу типов Num)
>>>>>>> upstream/master
double :: Num a => a -> a
double a = 2*a

-- б) Утроение заданного числа
--    (типовую аннотацию и образцы параметров следует написать самостоятельно)
triple :: Num a => a -> a
triple a = 3*a

-- в) Определение наибольшего из трёх заданных целых чисел (можно воспользоваться стандартной
--    двухаргументной функцией max).
max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

{-
  Проверка:
> max3 87 34 209
209
> max3 22 28 30
30
> max3 12 25 (-7)
25

-}

-- г) Функция, возвращающая True тогда и только тогда, когда оба ее аргумента равны True
-- (пользоваться стандартными логическими операциями не следует, обратите внимание на
--  образцы параметров функции, последняя строка -- "во всех остальных случаях").
bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _ _ = False


-- д) Функция, возвращающая True, если только один из её аргументов равен True,
-- и False в противном случае (пользоваться стандартными логическими операциями не следует).
oneTrue :: Bool -> Bool -> Bool
oneTrue True False = True
oneTrue False True = True
oneTrue _ _ = False

-- е) Дана температура в градусах Фаренгейта. Вычислить соответствующую температуру
-- в градусах Цельсия.
f2c :: Double -> Double
f2c a = (a-32)*5/9

{-
   ж) Найти наибольший общий делитель двух целых чисел, пользуясь
      алгоритмом Евклида (псевдокод):
      НОД(a, 0) = a.
      НОД(a, b) = НОД(b, a mod b), если b ≠ 0; 
-}
gcd' :: Integral a => a -> a -> a
gcd' a b = if b == 0 then a else gcd' b (mod a b)

-- з) Функция, возвращающая название дня недели по его номеру (от 1 до 7),
--    если номер неправильный, генерируется исключение (функция error).
--    В реализации следует пользоваться сопоставлением с образцами.
dayOfWeek :: Int -> String
dayOfWeek 1 = "Monday"
dayOfWeek 2 = "Tuesday"
dayOfWeek 3 = "Wednesday"
dayOfWeek 4 = "Thursday"
dayOfWeek 5 = "Friday"
dayOfWeek 6 = "Saturday"
dayOfWeek 7 = "Sunday"
dayOfWeek _ = error "Invalid argument"


-- Далее типовые аннотации, если их нет, следует писать самостоятельно.

-- 6) Условное определение функции

-- Пример.
-- Определение знака числа (-1, 0, 1). Класс типов Ord определяет операции сравнения.
sign :: (Num a, Ord a) => a -> Int
sign a
   | a < 0 = -1
   | a == 0 = 0
   | otherwise = 1

{-
   а) Найти значение функции f(x), вычисляемое по правилу:
          −x,   если x ≤ 0,
	  x^2,  если 0 < x < 2,
          4,    если x ≥ 2.
-}

eval_f :: Double -> Double
eval_f a
   | a <= 0 = -a
   | 0 < a && a < 2 = a^2
   | otherwise = 4

-- б) Написать функцию, возвращающую текстовую характеристику ("hot", "warm", "cool", "cold")
-- по заданному значению температуры в градусах Цельсия.
describeTemperature :: Double -> String
describeTemperature a
   | a >= 400 = "hot"
   | 300 <= a && a < 400 = "warm"
   | 200 <= a && a < 300 = "cool"
   | otherwise = "cold"

{- 
   в) (*) Дан список температур в градусах Фаренгейта. Вывести для каждого значения
    соответствующую текстовую характеристику.

  Решение:
> map (describeTemperature . f2c) [82, 94, 50, 65, 34]

  В этом решении с помощью операции (.) строится композиция (суперпозиция) функций
  и получившаяся функция применяется функцией map к каждому элементу списка.
-}

-- 7) Рекурсия

-- Пример. Вычислить сумму всех целых чисел от 1 до n (где n >= 1):
sum_n :: (Num a, Ord a) => a -> a
sum_n 1 = 1
sum_n n
  | n > 1 = n + sum_n (n-1)
  | otherwise = error "n should be >= 1"

-- а) Вычислить сумму всех целых чисел от a до b включительно.
sum_ab :: (Num a, Ord a) => a -> a -> a
sum_ab a b = sum_n b - sum_n a + a	

{-
   б) Числовая последовательность определяется следующим образом:
      a1 = 1, a2 = 2, a3 = 3, a_k = a_{k−1} + a_{k−2} − 2*a_{k−3}, k = 4, 5, ...
      Вычислить её n-й элемент.
-}
eval_a_n :: (Num a, Eq a) => a -> a
eval_a_n n = if n == 1 then 1 else if n == 2 then 2 else if n == 3 then 3 else eval_a_n (n-1) + eval_a_n (n-2) - 2*eval_a_n (n-3)

-- в) Вычислить, пользуясь рекурсией, n-ю степень числа a (n - целое):
pow :: (Num a, Num n, Ord n) => a -> n -> a	
pow a n = if n == 1 then a else a*pow a (n-1)

-- г) Пользуясь ранее написанной функцией pow, вычислить сумму: 1^k + 2^k + ... + n^k.
sum_nk :: (Num n, Eq n, Num k, Ord k) => n -> k -> n
sum_nk n k = if n == 1 then 1 else pow n k + sum_nk (n-1) k

-- д) Сумма факториалов чисел от 1 до n.
sum_fact 1 = 1
sum_fact n = fact n + sum_fact (n-1)
  where
    fact n = if n < 2 then 1 else n * fact (n-1) 

-- е) Количество цифр целого числа
number_digits :: (Integral n, Ord n) => n -> n
number_digits n
  | n < 0 = number_digits (-n)
  | -1 < n && n < 10 = 1
  | otherwise = 1 + number_digits (div n 10)

-- ж) Проверить, является ли заданное число простым.
isPrime :: (Integral n) => n -> Bool
isPrime n
  | n < 2 = False
  | n < 4 = True
  | otherwise = isProbablePrime n (n-1)
  where
    isProbablePrime n k = if k == 1 then True else mod n k /= 0 && isProbablePrime n (k-1)
	
-- 8) Разное

{-
   а) Дан номер года (положительное целое число). Определить количество дней в этом году,
  учитывая, что обычный год насчитывает 365 дней, а високосный — 366 дней. Високосным
  считается год, делящийся на 4, за исключением тех годов, которые делятся на 100 и
  не делятся на 400 (например, годы 300, 1300 и 1900 не являются високосными,
  а 1200 и 2000 — являются).
-}

nDays :: (Eq n, Integral n, Num n) => n -> n
nDays year = if isLeap year then 366 else 365
  where
    isLeap n = if mod n 400 == 0 then True else if mod n 100 == 0 then False else mod n 4 == 0
