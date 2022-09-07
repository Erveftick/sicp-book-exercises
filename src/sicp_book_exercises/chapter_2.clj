(ns ^{:doc          "Пространство имен, в котором собраны решения упражнений из книги
                     'Structure and Interpretation Computer Programs'
                     'Структура и интерпретация компьютерных программ'

                     https://newstar.rinet.ru/~goga/sicp/sicp.pdf

                     Раздел 2"
      :author       "Edward Kvashyn"
      :date-created "2022-09-07"
      :date-ended   "..."}
  sicp-book-exercises.chapter-2
  (:gen-class)
  (:require [sicp-book-exercises.chapter-1 :as ch-1]
            ))

; 2.1

(defn numer [x]
  (first x))

(defn denom [x]
  (comment
   "В книге эта функция описана через функцию cdr,
   которая является аналогом функции rest в Clojure.
   https://www.gnu.org/software/emacs/manual/html_node/eintr/car-_0026-cdr.html#:~:text=Clearly%2C%20a%20more%20reasonable%20name%20for%20cdr%20would%20be%20rest

   Но в Clojure нельзя перемножить списки, поэтому
   я буду использовать функцию second, чтобы взять
   конкретно число, а не список всех элементов
   кроме первого")
  (second x)
  )

(defn make-rat [n d]
  (comment
   "Аналогом cons из scheme является list. Функция
   cons в clojure тоже есть, но она вставляет
   элемент в начало списка")
  (let [g (ch-1/gcd n d)]
    (list (/ n g) (/ d g))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(comment
 (make-rat 1 -2) ; => (-1 2)
 (make-rat -1 -2) ; => (1 2)
 (make-rat -1 2) ; => (-1 2)
 (mul-rat (make-rat -1 2) (make-rat -1 -3)) ; => (-1 6)
 )

; 2.2

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn make-segment [start-segment end-segment]
  (let [[a b] start-segment
        [c d] end-segment]
    (list (ch-1/average a c) (ch-1/average b d))))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

(comment
 (print-point (make-segment '(-1 3) '(6 5)))
 ; (5/2,4)
 ; => nil
 )

; 2.3

(defn rect-sides
  "Принимает координаты прямоугольника,
  возвращает его ширину и высоту

  Координаты точек должны быть представлены
  в таком порядке:
  a----b
  |    |
  d----c"
  [a b c & d]
  (let [[x1 y1] a
        [x2 y2] b
        [x3 y3] c
        width  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))
        height (Math/sqrt (+ (Math/pow (- x2 x3) 2) (Math/pow (- y2 y3) 2)))]
    (list height width)))

(defn rect-square
  "Общая формула, которая возвращает площадь и
  периметр по поданным в функцию списку из
  высоты и ширины"
  [rect]
  (let [[height width] rect
        S (* height width)
        P (* 2 (+ height width))]
    {:square    S
     :perimeter P}))

(comment
 "Сначала я сделал функцию, которая просто считает
 площадь и периметр, когда в нее подаешь список с
 данными о высоте и ширине. Потом сделал функцию,
 которая считает стороны прямоугольника по поданным
 четырём точкам. В итоге всё свёл к одной функции,
 как и просилось"
 (rect-square '(3 5)) ; => {:square 15, :perimeter 16}
 (rect-square (rect-sides '(0 0) '(0 2) '(2 2) '(2 0))) ; => {:square 4.0, :perimeter 8.0}
 )