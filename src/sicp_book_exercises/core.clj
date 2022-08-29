(ns ^{:doc          "Пространство имен, в котором собраны решения для упражнений из книги
                     'Structure and Interpretation Computer Programs'
                     'Структура и интерпретация компьютерных программ'

                     https://newstar.rinet.ru/~goga/sicp/sicp.pdf"
      :author       "Edward Kvashyn"
      :date-created "2021-11-25"}
  sicp-book-exercises.core
  (:gen-class)
  (:require [oz.core :as oz]
            ))

; 1.1
(comment
  "Задание: Ниже приведена последовательность выражений. Какой результат напечатает интерпретатор в от-
  вет на каждое из них? Предполагается, что выражения вводятся в том же порядке, в каком они
  написаны."
  10
  (+ 5 3 4)
  (- 9 1)
  (/ 6 2)
  (+ (* 2 4) (- 4 6))
  (def a 3)
  (def b (+ a 1))
  (+ a b (* a b))
  (= a b)
  (if (and (> b a) (< b (* a b))) b a)
  (cond
    (= a 4) 6
    (= b 4) (+ 6 7 a)
    :else 25)
  (+ 2 (if (> b a) b a)))

; 1.2
(comment
  "Задание: Переведите следующее выражение в префиксную форму:"
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7))))

; 1.3
(defn task-1-3
  "Задание: Определите процедуру, которая принимает в качестве аргументов три числа и возвращает сумму
квадратов двух бо́льших из них"
  [a b c]
  (let [max (max a b c)
        min (min a b c)
        avrg (- (+ a b c) max min)]
    (+ (* max max) (* avrg avrg))))

(comment
  (task-1-3 5 1 0))

; 1.4
(defn a-plus-abs-b
  "Задание: Заметим, что наша модель вычислений разрешает существование комбинаций, операторы кото-
рых — составные выражения. С помощью этого наблюдения опишите, как работает следующая
процедура:"
  [a b]
  ((if (> b 0) + -) a b))

(comment
  (str "Ответ: Функция которая в зависимости от второго входящего параметра опеределяет,
       какая математическая операция будет выполнена. Если число положительно - сумма,
       иначе - отнимание. Такой себе макрос")
  (a-plus-abs-b 1 2)
  (a-plus-abs-b 3 -2))


; 1.5
(defn p-1-5 []
  (p-1-5))
(defn test-1-5 [x y]
  (if (= x 0)
    0
    y))

(comment
  "Задание: Бен Битобор придумал тест для проверки интерпретатора на то, с каким порядком вычислений он
работает, аппликативным или нормальным. Бен определяет такие две процедуры: (выше)

Затем он вычисляет выражение:"

  (test-1-5 0 (p-1-5))

  "Какое поведение увидит Бен, если интерпретатор использует аппликативный порядок вычислений?
Какое поведение он увидит, если интерпретатор использует нормальный порядок? Объясните Ваш
ответ. (Предполагается, что правило вычисления особой формы if одинаково независимо от того,
какой порядок вычислений используется. Сначала вычисляется выражение-предикат, и результат
определяет, нужно ли вычислять выражение-следствие или альтернативу.)")

"Ответ: Функция р это рекурсивная функция, а в случае аппликативного порядка вычисления функция
 будет подставлять функцию, не добравшись до значения, потому как сказано в книге:
 '[аппликативн.]она подставляет на место параметров выражения-операнды, пока не получит выраже-
 ние, в котором присутствуют только элементарные операторы'. Соответственно, мы просто не
 войдем в тест, потому что не сможем получить значение от p

 Если мы рассматриваем нормальный порядок, то (test-1-5 0 (p-1-5)) у нас превратится, по-сути,
 в (if (= 0 0) 0 (p))"

; 1.6

(defn square
  "Базовая функция из учебника SICP"
  [x]
  (* x x))

(defn liza:new-if
  "Функция, которую написала Лизина подруга Ева Лу Атор"
  [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

(defn good-enough?
  "Базовая функция из учебника SICP

  Определяет, является ли число достаточно точным"
  [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn average
  "Базовая функция из учебника SICP

  Считает среднее арифметическое двух чисел"
  [x y]
  (/ (+ x y) 2))

(defn improve
  "Базовая функция из учебника SICP

  Улучшает число для дальнейших исчислений"
  [guess x]
  (average guess (/ x guess)))

(defn liza:sqrt-iter
  "Главная функция, на примере которой нужно объяснить, что будет если использовать
  функцию написанную подругой Лизы liza:new-if

  Задание:
  Что получится, когда Лиза попытается использовать процедуру liza:new-if
  для вычисления этой функции?"
  [guess x]
  (liza:new-if
    (good-enough? guess x)
    guess
    (liza:sqrt-iter
      (improve guess x)
      x)))

(comment
  (liza:new-if (= 3 3) 0 5)

  ; НЕ ЗАПУСКАТЬ ФОРМУ НИЖЕ
  (liza:sqrt-iter 24 5)

  "Ответ: произойдет бесконечная рекурсия.
   Так как liza:sqrt-iter использует аппликативный метод исчисления, то cond
   попробует подставить числа (и только элементарные операторы), чтобы потом получить ответ,
   но как и в задаче выше её ждёт неудача, так как cond не элементарный оператор, и
   рекурсия не даст число")


; 1.7

(defn good-enough?-2
  "Улучшенная функция по определению достаточно ли точно число

  Функция требует в задании 1.7"
  [guess previous-guess]
  (< (Math/abs (- guess previous-guess)) 0.00000000001))

(defn sqrt-iter
  "Базовая функция из учебника SICP

  Считает корень квадратный с определенной точностью"
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt-iter-2 [guess x]
  (if (good-enough?-2 guess (improve guess x))
    guess
    (sqrt-iter-2 (improve guess x) x)))

(defn sqrt
  "Базовая функция из учебника SICP

  Считает корень квадратный"
  [x]
  (sqrt-iter 1.0 x))

(defn sqrt-2
  "Улучшенная функция для задания 1.7 учебника SICP.
  Считает корень квадратный для бОльших чисел чем та,
  что описана в книге"
  [x]
  (sqrt-iter-2 1.0 x))

(comment
  (Math/sqrt 12345678901234)
  (sqrt 1234567890123)                                      ; Это число самое большое из возможных (по знакам)
  (sqrt-2 12345678901234)                                   ; Новая функция уже справляется с бОльшим количеством ;)
  )


; 1.8

(defn good-enough-cube? [guess x]
  (< (Math/abs (- (* guess guess guess) x)) 0.00001))

(defn improve-cube
  "Функция из задания 1.8 SICP"
  [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(defn qbrt-iter
  [guess x]
  (if (good-enough-cube? guess x)
    guess
    (qbrt-iter (improve-cube guess x) x)))

(defn qbrt
  "Функция-ответ на ответ 1.8"
  [x]
  (qbrt-iter 1.0 x))

(comment
  (qbrt 9)
  (Math/pow 9 (/ 3)))


; --- Минутка самостоятельного исчисления факториала ---

(comment
  (Math/exp (+ (Math/log 2) (Math/log 2)))
  (reduce * (drop 1 (range 6)))
  (drop 1)
  (map #(+ 1 %) (range 3))
  (reduce * '(1 2 3)))

(defn facto [x]
  (if (= x 1)
    1
    (reduce * (map #(+ 1 %) (range x)))))

(comment
  (facto 6))

; --- ---


; 1.9

#_(define (+ a b)
          (if (= a 0)
            b
            (inc (+ (dec a) b))))

(comment
  "Проиллюстрированый процесс функции выше"
  (+ 4 5)
  (inc (+ 3 5))
  (inc (inc (+ 2 5)))
  (inc (inc (inc (+ 1 5))))
  (inc (inc (inc (inc (+ 0 5)))))
  (inc (inc (inc (inc 5))))
  (inc (inc (inc 6)))
  (inc (inc 7))
  (inc 8)
  9)
; => recursion

#_(define (+ a b)
          (if (= a 0)
            b
            (+ (dec a) (inc b))))

(comment
  "Проиллюстрированый процесс функции выше"
  (+ 4 5)
  (+ (dec 4) (inc 5))
  (+ 3 6)
  (+ (dec 3) (inc 6))
  (+ 2 7)
  (+ (dec 2) (inc 7))
  (+ 1 8)
  (+ (dec 1) (inc 8))
  (+ 0 9)
  9)
;=> iterative


; 1.10
(defn A
  "Базовая функция из учебника SICP

  Процедура вычисляет математическую функцию, называемую функцией Аккермана."
  [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))))

(A 1 10)                                                    ;=> 1024
(A 2 4)                                                     ;=> 65536
(A 3 3)                                                     ;=> 65536

(comment
  "В задании ниже я видимо был увлечен познанием Clojure и сделал
  функции по обсчету того, что делает функция Аккермана. В самом
  же задании нигде не просилось написать свои функции, только указать
  эквивалент в мат. формулах. Писал я это давно, так что уже не
  вспомню, что именно меня сподвигло написать свои функции ;)")

;(define (k n) (* 5 n n))
(defn k
  "Функция эквивалентна к 5*n^2"
  [n] (* 5 n n))

;(define (f n) (A 0 n))
(defn f
  "Функция эквивалентна к 2*n"
  [n] (* 2 n))

;(define (g n) (A 1 n))
(defn g
  "Функция эквивалентна к 2^n"
  [n]
  ; I could use just Math/pow func, but it was pretty easy,
  ; so I made my own func
  (reduce * (for [_ (range n)] 2)))

;(define (h n) (A 2 n))
(defn h
  "Функция эквивалентна к 2^2^...^2"
  [n]
  (loop [num n
         sum [2]]
    (if (> num 0)
      (recur (dec num) (conj sum (square (last sum))))
      (last sum))))

(comment
  (A 0 3)                                                   ;=> 6
  (f 3)                                                     ;=> 6

  (A 1 3)                                                   ;=> 8
  (A 0 (A 0 (A 1 1)))                                       ;=> 8
  (g 3)                                                     ;=> 8

  (A 2 3)                                                   ;=> 16
  (A 1 (A 1 (A 2 1)))                                       ;=> 16
  (A 1 (A 0 2))                                             ;=> 16
  (g 4)                                                     ;=> 16

  (h 4)                                                     ;=> 65536
  (A 2 4)                                                   ;=> 65536
  )



; 1.11
(defn f-recursive
  "Напишите процедуру, вычисляющую f с помощью рекурсивного процесса."
  [n]
  (if (< n 3)
    n
    (+ (f-recursive (- n 1)) (f-recursive (- n 2)) (f-recursive (- n 3)))))

(defn f-iterative-iter
  [a b c count]
  (if (= count 0)
    c
    (f-iterative-iter (+ a b c) a b (dec count))))

(defn f-iterative
  "Напишите процедуру, вычисляющую f с помощью итеративного процесса."
  [n]
  (f-iterative-iter 2 1 0 n))

(comment
  (f-recursive 40)                                          ; Для 40 я ответ ждал болше 10 секунд, потом бросил
  (f-iterative 40)                                          ; Этот дает мгновенно ;)
  )


; 1.12

(defn pascal-triangle-row-finder
  "Главная функция для pascal-triangle-finder"
  [coll]
  (let [row-body (mapv #(+ %1 %2) (butlast coll) (rest coll))]
    (cons 1 (conj row-body 1))))

(defn pascal-triangle-finder
  "Напишите процедуру, вычисляющую элементы треугольника Паскаля
  с помощью рекурсивного процесса."
  [row-count]
  (loop [rows ['(1) '(1 1)]]
    (if (= (count rows) row-count)
      rows
      (recur (conj rows (pascal-triangle-row-finder (last rows)))))))

(comment
  (pascal-triangle-finder 7)
  )


; 1.13

(def sqrt-5 (Math/sqrt 5))
(def psi (/ (- 1 sqrt-5) 2))
(def fi (/ (+ 1 sqrt-5) 2))

(defn Fib
  "Функция из задания 1.13, которая выводит в консоль число fi,
  от которого требовалось найти ближайшее к нему целое число.
  Также выведен сам ответ в double формате, для показа, что это
  действительно целое число, а (int answer) сделано для
  удобства чтения самого ответа"
  [n]
  (let [fi-1 (Math/pow fi n)
        fi-2 (/ fi-1 sqrt-5)
        float-expr (/ (- fi-1 (Math/pow psi n)) sqrt-5)]
    (println "Fi: " fi-2)
    (println "nearest to Fi: " (Math/round fi-2))
    (println "Real answer (without taking int part): " float-expr)
    (int float-expr)))

(comment
  "Это задание больше математическое, чем программисстское.
  С помощью определения чисел Фибоначчи (см. раздел 1.2.2) и индукции
  докажите, что Fib(n) = (fi^n - psi^n) / √5 есть целое число,
  ближайшее к fi^n / √5.

  1) При n=1:"
  (Fib 1)
  ; Fi:  0.7236067977499789
  ; nearest to Fi:  1
  ; Real answer (without taking int part):  1.0
  ;=> 1
  "Ответ является целым, из последовательности Фибоначчи и ближайшим
  к fi является сам ответ.
  2) При n=2:"
  (Fib 2)
  ; Fi:  1.1708203932499368
  ; nearest to Fi:  1
  ; Real answer (without taking int part):  1.0
  ; => 1
  "Ответ является целым, из последовательности Фибоначчи и ближайшим
  к fi является сам ответ.

  3) При n+1:
  Предположим что Fib(n) = (fi^n - psi^n) / √5 удовлетворяет условию.
  Следует доказать, что при Fib(n+1) = (fi^(n+1) - psi^(n+1)) / √5
  истинно.

  Fib(n+1) = Fib(n) + Fib(n-1) => ... тут я делал рассчеты на листике,
  еще сразу стоит учесть тот факт, что fi^2 = ... = fi + 1, рассчеты
  также на листике, но аналогично для psi^2 = ... = psi + 1 и получилось
  следующее
  => (fi^(n-1)*fi^2 - psi^(n-1)*psi^2)/ √5 = (fi^(n+1) - psi^(n+1))/ √5
  Соответственно, утверждение верно для n+1. Индукция выполняется для
  всех целых чисел N.

  ЧТД")


; 1.14

(defn first-denomination
  "Базовая функция из учебника SICP"
  [kinds-of-coins]
  (cond
    (= kinds-of-coins 1) 1
    (= kinds-of-coins 2) 5
    (= kinds-of-coins 3) 10
    (= kinds-of-coins 4) 25
    (= kinds-of-coins 5) 50))

(defn cc
  "Базовая функция из учебника SICP"
  [amount kinds-of-coins]
  (cond
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else
    (let [sum-w-out-first (cc amount (- kinds-of-coins 1))
          sum-minus-d (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)]
      (+ sum-w-out-first sum-minus-d))))

(def cc2-atom (atom ()))

(defn cc2
  [amount kinds-of-coins iteration]
  (swap! cc2-atom conj {:amount amount :kinds-of-coins kinds-of-coins :iteration iteration})
  (cond
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else
    (let [sum-w-out-first (cc2 amount (dec kinds-of-coins) (inc iteration))
          sum-minus-d (cc2 (- amount (first-denomination kinds-of-coins)) kinds-of-coins (inc iteration))]
      (+ sum-w-out-first sum-minus-d))))

(defn count-change
  "Базовая функция из учебника SICP"
  [amount]
  (cc amount 5))

(comment
  "Задание(первая часть): Нарисуйте дерево, иллюстрирующее процесс,
  который порождается процедурой count-change из раздела 1.2.2
  при размене 11 центов."
  (count-change 11)

  "Решение: я сделал свою функцию (сс2), которая отличается от той,
   что описана в учебнике тем, что я создал cc2-atom атом, который
   обновляется каждый раз, как вызывается функция. Чтобы поймать
  разветвления дерева, я добавил еще переменную итерации, чтобы
  понимать, какое это разветвление, грубо говоря, по счёту. Потом
  отсортировали по итерациям и данные для дерева в +- удобном виде.
  Из этих данных нарисовать дерево означает просто каждую первую
  в левую ветку вынести, вторую в правую. Нарисовал на листике,
  получилось много, поэтому рекомендую просто запустить сначала
  функцию сс2, а после этого функцию сортировки."
  (reset! cc2-atom ())
  (cc2 100 5 0)
  (sort-by :iteration @cc2-atom)

  "Задание(вторая часть): Каковы порядки роста памяти и числа
  шагов, используемых этим процессом при увеличении суммы,
  которую требуется разменять?"

  "Ответ: так как у нас рекурсивный метод, то количество памяти
  будет пропорционально высоте дерева. Самый длинный случай будет
  тогда, когда вам разменяют ваши n гривен по 1 копейке.
  Получается что память растет линейно O(n).

  Для количества шагов я вывел закономерность для (cc n 1), что
  всегда равнялось 2n+1 шагам. Для квадрата мне пришлось лезть
  в интернет, чтобы зацепиться за идею для этой задачи: исходя
  из рассчетов, порядок числа шагов для (cc n 2) составляет n^2.
  Для текущей задачи ответ O(n^5) шагов"
  )


; 1.15
(def atom:sine-operations-count (atom 0))
(defn cube
  "Базовая функция из учебника SICP"
  [x]
  (* x x x))
(defn p
  "Базовая функция из учебника SICP"
  [x]
  (swap! atom:sine-operations-count inc)
  (- (* 3 x) (* 4 (cube x))))
(defn sine
  "Базовая функция из учебника SICP"
  [angle]
  (if (not (> (Math/abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(comment
  "Задача:
  а. Сколько раз вызывается процедура p при вычислении (sine 12.15)?
  б. Каковы порядки роста в терминах количества шагов и используемой
  памяти (как функция a)для процесса, порождаемого процедурой
  sine при вычислении (sine a)?"

  "Ответы:
  а) Просто запускаем функцию снизу. Ответ 5"
  (do
    (reset! atom:sine-operations-count 0)
    (sine 12.15)
    @atom:sine-operations-count)

  "б) Так как на каждой итерации мы делим угол на 3 пока он не будет
  меньше 0.1, то оно уменьшится в 3^n раза. Получается неравенство:
  0.1 > a/(3^n). Эту запись можем свести к логарифмам, получится:
  n < log3(10a). Ну и т.к тут логарифм, то число шагов растет с
  O(log a). Тоже самое и с памятью, логарифмически."
  )


; 1.16
(defn task-1-16-iter [b n a]
  (cond
    (= n 0) a
    (even? n) (task-1-16-iter (square b) (/ n 2) a)
    :else (task-1-16-iter b (dec n) (* a b))))

(defn task-1-16 [b n]
  (task-1-16-iter b n 1))

(comment
  "Задание: Напишите процедуру, которая развивается в виде итеративного
  процесса и реализует возведение в степень за логарифмическое число
  шагов, как fast-expt."

  (task-1-16 2 13)
  )

; 1.17
(defn double-1-17
  "Функция для задания 1.17
  Должна была называться double, но такое имя уже есть у Clojure"
  [a]
  (* a 2))

(defn halve-1-17
  "Функция для задания 1.17"
  [a]
  (/ a 2))

(defn task-1-17
  "Умножает числа итеративно используя удвоение и деление на 2"
  [a b]
  (cond
    (= b 0) 0
    (= b 1) a
    (even? b) (double-1-17 (task-1-17 a (halve-1-17 b)))
    :else (+ a (task-1-17 a (dec b)))))

(comment
  (task-1-17 2 6))

; 1.18
(defn task-1-18-iter
  "Насколько я понял, нужно чтобы мы решение 1.17 переписали с
  третьей переменной (в 1.16 это была переменная а)"
  [a b n]
  (cond
    (= b 0) n
    (even? b) (task-1-18-iter (double-1-17 a) (halve-1-17 b) n)
    :else (task-1-18-iter a (dec b) (+ a n))))

(defn task-1-18 [a b]
  (task-1-18-iter a b 0))

(comment
  "Задание: Разработайте процедуру, которая порождает итеративный
   процесс для умножения двух чисел с помощью сложения, удвоения и
   деления пополам, изатрачивает логарифмическое число шагов"

  (task1p18 3 6))

; 1.19
(defn fib-iter-1-19 [a b p q count]
  (cond
    (= count 0) b
    (even? count) (fib-iter-1-19 a
                                 b
                                 (+ (* p p) (* q q))        ; вычислить p’
                                 (+ (* q q) (* 2 p q))      ; вычислить q’
                                 (/ count 2))
    :else (fib-iter-1-19 (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1))))

(defn fib-1-19
  [n]
  (fib-iter-1-19 1 0 0 1 n))

(defn a1
  "Попытка схитрить на этом задании использовав а1 для
  вычисления каждого из слогаемых, но которая в процессе
  написания натолнула на идею сокращения к более простой
  форме"
  [a b]
  (let [p 0
        q 1
        am (+ (* b q) (* a q) (* a p))
        bm (+ (* b p) (* a q))]
    {:a (+ (* bm q) (* am q) (* am p))
     :b (+ (* bm p) (* am q))}))

(comment
  (a1 2 4)
  (a1 20 12)

  "Ответ в блокноте на странице 32

  В этом задании в процессе понимания, что же нужно от меня,
  начал писать функцию, что есть 'в лоб' и потом понял что
  нужна алгебра. Подставили одно в другое, объединили и
  вынесли а и b за скобки, получили некоторую функцию, из
  которой вскоре получилось понять, что это именно то, что
  нам нужно. По заданию нужно было найти p' и q'

  resources/images/1.19.png

  Финальные слагаемые:
  ( b(q^2+2pq) + a(q^2+p^2) + a(q^2+2pq); b(q^2+p^2) + a(q^2+2pq) )
  Если посмотреть в формулу, то замечаем, что
  p' = q^2+p^2, a q' = q^2+2pq. Потом эти формулы вставили в
  функцию и всё заработало
  "

  (Fib 7)
  (fib-1-19 7)
  )

; 1.20
(defn gcd
  "Базовая функция из учебника SICP

  Процесс, порождаемый процедурой, разумеется, зависит от того,
  по каким правилам работает интерпретатор. В качестве примера
  рассмотрим итеративную процедуру gcd, приведенную выше."
  [a b]
  (println {:a a :b b})
  (if (= b 0)
    a
    (gcd b (mod a b))))

(comment
  "Задача: Используя подстановочную модель для нормального порядка,
   проиллюстрируйте процесс, порождаемый при вычислении (gcd 206 40)
   и укажите, какие операции вычисления остатка действительно
   выполняются. Сколько операций remainder выполняется на самом
   деле при вычислении(gcd 206 40) в нормальном порядке?
   При вычислении в аппликативном порядке?"

  "Ответ
  resources/images/1.20(normal).png
  resources/images/1.20(applicative).png

  В нормальном порядке функция (mod) исполнится 18 раз
  В Аппликативном 4 раза")

; 1.21

(defn divides?
  "Базовая функция из учебника SICP"
  [a b]
  (= (mod b a) 0))

(defn find-divisor
  "Базовая функция из учебника SICP"
  [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor
  "Базовая функция из учебника SICP"
  [n]
  (find-divisor n 2))

(defn prime?
  "Базовая функция из учебника SICP"
  [n]
  (= n (smallest-divisor n)))

(comment
  (smallest-divisor 199)                                    ; => 199
  (smallest-divisor 1999)                                   ; => 1999
  (smallest-divisor 19999)                                  ; => 7
  )


; 1.22

(defn report-prime
  "Базовая функция из учебника SICP"
  [n elapsed-time]
  (println (str n " *** " elapsed-time)))

(defn start-prime-test
  "Базовая функция из учебника SICP"
  [n start-time]
  (when (prime? n)
    (report-prime n (/ (double (- (. System (nanoTime)) start-time)) 1000000.0))))

(defn timed-prime-test
  "Базовая функция из учебника SICP"
  [n]
  (start-prime-test n (. System (nanoTime))))

(defn find-primes [coll]
  (when-not (empty? coll)
    (timed-prime-test (first coll))
    (find-primes (rest coll))))

(defn search-for-primes
  "Функция, которая требуется в задании 1.22

  Ищет первые три простых числа в заданном диапазоне.
  Каждый раз как находит число пишет затраченное на это время"
  [start finish]
  (let [numbers (->> (range start finish) (filter odd?))]
    (find-primes numbers)
    ))

(comment
  "По условию, нужно использовать те функции, которые прописаны в задании,
  но код написанный на Schema нужно было переписать на Clojure, и ближайшей
  возможной функцией к (runtime) из Schema я подумал что это
  (. System (nanoTime))."
  (timed-prime-test 7)
  (search-for-primes 1000 1100)
  (search-for-primes 10000 10100)
  (search-for-primes 100000 100100)
  (search-for-primes 1000000 1000100)

  "Еще вопросы:
   1) Подтверждают ли это Ваши √n замеры времени?
   - Система определения времени, на выполнение программы, скажем, не идеальна.
   Бывает так, что первое число считает за 3 милисекунды, второе и третье за 1,
   что довольно странный тренд, но чаще всего 0.хххх. Так что не подтверждает

   2) Хорошо ли поддерживают предсказание √n данные для 100 000 и 1 000 000?
   - Не могу сказать. Для 100 000 и для 1000000 нельзя четко сказать, что
   для миллиона времени будет затрачено больше. Потому что запустив 3 раза
   функцию получил абсолютно разные цифры, где для определения простое ли
   число для 100 000 оно считало дольше, чем для миллиона. Но если возьмем
   в среднем, то нет, предсказание не верно. Так, для числа 100003 нужно
   0.302629 милисек времени, а при определении, простое ли число для всех
   чисел больше миллиона операция не доходила до 0.5, хотя если мы умножим
   0.302629 * √10 = 0.95. То есть почти в 2 раза больше от среднего реального

   3) Совместим ли Ваш результат с предположением, что программы на Вашей машине
    затрачивают на выполнение задач время, пропорциональное числу шагов?
    - Нет"
  )

; 1.23

(defn next-1-23
  "Функция требуемая в задании 1.23"
  [n]
  (if (= n 2) 3 (+ n 2)))

(defn find-divisor-2
  "Базовая функция из учебника SICP"
  [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor-2 n (next-1-23 test-divisor))))

(defn smallest-divisor-2
  "Базовая функция из учебника SICP"
  [n]
  (find-divisor-2 n 2))


(defmacro timer-1
  "Аналог макроса time, выводит только время исполнения программы"
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn average-in-coll
  "Вспомогательная функция, которой нет в книге.

  Считает среднее арифметическое по коллекции"
  [coll]
  (/ (reduce + coll) (count coll)))

(defn transpose
  "Вспомогательная функция, которой нет в книге.

  Переводит вектор векторов в приемлимый для
  меня формат"
  [coll]
  (apply map vector coll))

(comment
  "Ответ: Ниже я написал функцию, которая определяет время при
  определении на простоту числа стандартной функией и улучшенной.
  Потом считает на сколько % ускорилась функция."
  (let [prime-numbers '(1009 1013 1019 10007 10009 10037
                         100003 100019 100043 1000003 1000033 1000037)]
    (->>
      (for [num prime-numbers]
        {:usual (timer-1 (smallest-divisor num))
         :fast  (timer-1 (smallest-divisor-2 num))})
      (mapv
        (fn [e]
          (let [{:keys [usual fast]} e]
            (let [diff (format "%.2f" (* (/ usual fast) 100))]
              (assoc e :diff (str diff "%"))))))))

  "Ниже функция, которой я пытался посчитать средний прирост по каждому
   числу за 100 итераций, чтобы не руками. Но и эту функцию по хорошему
   нужно апроксимировать ;). Итого, сказать, что функция улучшает в 2 раза
   я не могу, потому что среднее число у меня получилось ~152%. Иногда
   больше 200, иногда меньше 150, но не ниже 145.

   Почему не 200%? Мне кажется что нужно уменьшать количество if-ов и
   прочих проверок. Ну и снова прогонять."
  (let [prime-numbers '(1009 1013 1019 10007 10009 10037
                         100003 100019 100043 1000003 1000033 1000037)
        data (group-by
               :num
               (flatten
                 (for [i (range 101)]
                   (for [num prime-numbers]
                     {:num   num
                      :usual (timer-1 (smallest-divisor num))
                      :fast  (timer-1 (smallest-divisor-2 num))}))))]
    (str (format
           "%.2f"
           (average-in-coll
             (map average-in-coll
                  (transpose
                    (for [d data]
                      (mapv
                        (fn [e]
                          (let [{:keys [usual fast]} e]
                            (let [diff (* (/ usual fast) 100)]
                              diff)))
                        (val d))
                      )))))
         "%"))
  )

; 1.24
(defn expmod [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (mod
                  (square (expmod base (/ exp 2) m))
                  m)
    :else (mod
            (* base (expmod base (- exp 1) m))
            m)))

(defn try-it [a n]
  (= (expmod a n n) a))

(defn fermat-test [n]
  (try-it (inc (rand-int (dec n))) n))

(defn fast-prime? [n times]
  (cond
    (= times 0) true
    (fermat-test n) (fast-prime? n (- times 1))
    :else false))

(comment
  "Задание: Модифицируйте процедуру timed-prime-test из упражнения 1.22
   так, чтобы она использовала fast-prime? (метод Ферма) и проверьте
   каждое из 12 простых чисел, найденных в этом упражнении."

  "Ответ: я думаю досаточно того, что мы воспользуемся встроенной в
  Clojure функицей time для более точного определения времени"
  (time (fast-prime? 1009 100))

  "Еще вопрос: Исходя из того, что у теста Ферма порядок роста O(log n),
   то какого соотношения времени Вы бы ожидали между проверкой на
   простоту поблизости от 1 000 000 и поблизости от 1000?
   - Логарифмической, конечно же.
   Я визуализировал данные, и на маленьком отрезке это выглядит всё
   довольно скудно, но стоит учесть, что мы взяли 12 чисел, поэтому
   никакой апроксимации тут и не могло быть, но график построен. Если
   же сделать проверку на простые числа с 1000 до миллиона, то я
   уверен, что оно покажет +- логарифимический график

   Сам график тут:
   images/visualization-1.24.png
   "

  (defn task-1-24 []
    (let [prime-numbers '(1009 1013 1019 10007 10009 10037 100003
                           100019 100043 1000003 1000033 1000037)
          data (group-by
                 :num
                 (flatten
                   (for [i (range 101)
                         num prime-numbers]
                     {:num         num
                      :fast-prime? (timer-1 (fast-prime? num 1000))})))]
      (->> data
           (map
             (fn [num-info]
               {:key (key num-info)
                :val (average-in-coll
                       (mapv
                         (fn [e]
                           (-> e :fast-prime?))
                         (val num-info)))}))
           (sort-by :key)
           )))

  (task-1-24)

  "Всё приведенное ниже нужно для отображения графика. Если интересно,
  то закидывать в репл поочередно сверху вниз"
  (oz/start-server!)

  (def line-plot
    {:data     {:values (task-1-24)}
     :encoding {:x {:field "val" :type "quantitative"}
                :y {:field "key" :type "quantitative"}}
     :mark     "line"})

  ;; Render the plot
  (oz/view! line-plot)
  )


; 1.25

(defn fast-expt [b n]
  (cond
    (= n 0) 1
    (even? n) (square (fast-expt b (/ n 2)))
    :else (* b (fast-expt b (- n 1)))))

(defn expmod:liza [base exp m]
  (mod (fast-expt base exp) m))

(comment
  "Снизу представлены функции и функция time для опеределения,
  какая из функций будет работать быстрее(а значит и эффективнее).
  Но запустив функцию из этого задания, получил ошибку:

  Execution error (ArithmeticException) at java.lang.Math/multiplyExact (Math.java:948).
  long overflow

  Посмотрев в реализацию алгоритма из задания, можно заметить,
  что тут каждое число будет возводится в квадрат. После чего
  снова возводится в квадрат, и так пока не достигнет желаемого
  результата. Если мы функцию square запишем так:"
  (defn square-2 [x]
    (println x)
    (* x x))
  "... то мы можем посмотреть какие числа возводит в квадрат
  предложенный метод. Это числа 3, 27, 729, 1594323, 2541865828329.
  Оригинальный же метод считает в свою очередь такие числа:
  3, 27, 40, 30, 52. Понятное дело считать квадрат для 52 проще,
  чем для 2541865828329. Поэтому Лиза П. Хакер зря жалуется. Она
  не права. Для проверки простых чисел эту функцию использовать
  точно нельзя, потому что очень неэффективная функция."

  (time (expmod 3 53 53))
  (Math/pow 3 14)
  (time (expmod:liza 3 53 53)))

; 1.26
(comment
  "Проблема кода Хьюго в том, что кваждый раз, когда получается
  чётная степень в expmod, то обсчёт идёт не 1 раз, а два.
  Допустим у нас четная степень, а функция (expmod base 2n m)
  требует k шагов. Порядок действий такой:"

  "  (square (expmod base 2n m)) ->
     (square (square (expmod base n m)))

     Кол-во шагов: k+1 -> (k+1)+1"

  "Для функции Хьюго это будет выглядеть так:"

  "  (* (expmod base 2n m) (expmod base 2n m)) ->
     (* (* (expmod base n m)
           (expmod base n m))
        (* (expmod base n m)
           (expmod base n m)))

     Кол-во шагов: 2k -> 4k"

  "Сложность первого O(log n).
  Второго O(log 2^n) => O(n log2) => O(log n)"
  )

; 1.27

(defn carmichael-fn [n]
  (defn iter [a]
    (cond
      (= a 1) true
      (not (try-it a n)) false
      :else (iter (dec a))))
  (iter (dec n)))

(comment
  (let [carmichaels-nums [561, 1105, 1729, 2465, 2821, 6601]]
    (for [num carmichaels-nums]
      (carmichael-fn num))
    )
  ; => (true true true true true true)
  )

; 1.28
(defn square-checker [x m]
  (if
    (and (not (or (= x 1) (= x (- m 1))))
         (= (mod (* x x) m) 1))
    0
    (mod (* x x) m)))

(defn miller-expmod [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (square-checker (miller-expmod base (/ exp 2) m) m)
    :else (mod (* base (miller-expmod base (- exp 1) m))
               m)))

(defn miller-rabin-fn [n]
  (defn mil-rab-try-it [a]
    (= (miller-expmod a (- n 1) n) 1))
  (mil-rab-try-it (+ 2 (rand-int (- n 2)))))

(comment
  (let [prime-numbers '(1009 1013 1019 10007 10009 10037 100003
                         100019 100043 1000003 1000033 1000037
                         561 562 1105 1729 2465 2821 6601)]
    (for [num prime-numbers]
      {:num    num
       :prime? (miller-rabin-fn num)}))
  )

; 1.29

(defn simpson-law [f a b n]
  (let [h (/ (- b a) n)]
    (* (/ h 3)
       (reduce
         +
         (let [y-0 (f (+ a (* 0 h)))
               y-n (f (+ a (* n h)))]
           (conj
             (for [i (range 1 n)]
               (let [y-i (f (+ a (* i h)))]
                 (if (even? i)
                   (* 2 y-i)
                   (* 4 y-i)
                   )
                 ))
             y-0 y-n))))))

(comment
  (simpson-law cube 0 1 100)                                ; => 1/4
  (simpson-law cube 0 1 1000)                               ; => 1/4
  "Наша функция точнее"
  )

; 1.30

(defn sum-- [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum-- term (next a) next b))))

(defn sum-1-30 [term a next b]
  (defn sum-iter [a result]
    (if (> a b)
      result
      (sum-iter (next a) (+ result (term a)))))
  (sum-iter (term a) 0))

(comment
  (sum-- cube 1 inc 10)                                     ; => 3025
  (sum-1-30 cube 1 inc 10)                                  ; => 3025
  )

; 1.31

(defn product-recursive [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-recursive term (next a) next b))))

(defn product-iterative [term a next b]
  (defn prod-iter [a result]
    (if (> a b)
      result
      (prod-iter (next a) (* result (term a)))))
  (prod-iter (term a) 1))

(defn factorial-1-31 [b]
  (product-recursive identity 1 inc b))

(defn plus-2 [x]
  (+ 2 x))

(defn pi-definer [precise]
  (let [chislitel (* (square (product-recursive identity 4 plus-2 (dec precise))) 2 precise)
        znemenatel (square (product-recursive identity 3 plus-2 precise))]
    (double (* 4 (/ chislitel znemenatel)))))

(comment
  "Часть а:"
  (product-recursive identity 1 inc 10)
  (factorial-1-31 10)
  (pi-definer 20)

  "Часть б"
  (product-iterative identity 1 inc 10)
  )

; 1.32

(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(defn accumulate-iterative [combiner null-value term a next b]
  (defn accumulate-iter [a result]
    (if (> a b)
      result
      (accumulate-iter (next a) (combiner result (term a)))))
  (accumulate-iter (term a) null-value))

(comment
  "а:"
  (sum-1-30 cube 1 inc 10)
  (accumulate + 0 cube 1 inc 10)

  (product-recursive identity 1 inc 10)
  (accumulate * 1 identity 1 inc 10)

  "б:"
  (accumulate-iterative * 1 identity 1 inc 10)
  )

; 1.33

(defn filtered-accumulate [combiner null-value term a next b filter-fn]
  (let [term-value? (if (filter-fn a) (term a) null-value)]
    (if (> a b)
      null-value
      (combiner term-value?
                (filtered-accumulate combiner null-value term (next a) next b filter-fn)))))

(defn helper-1-33 [n a]
  (and (< a n) (= 1 (gcd a n))))

(comment
  "a:"
  (filtered-accumulate + 0 square 1 inc 5 prime?)

  "б:"
  (filtered-accumulate * 1 identity 1 inc 5 (partial helper-1-33 7))
  )