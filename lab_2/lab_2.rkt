(newline)
(display "---------Task 1---------")
(newline)
(newline)


;(display (atan 0.5) )

(define (iter i) ;Фунція що проходить ітерації значень [0, 3] для вбудованих функцій
  (cond
    ((and (>= i 0) (<= i 3))
      (display i)
      (display " = ")
      (display (test_arctg i))
      (newline)
      (iter (+ i 0.5))
    )
    (else 
      (display "")
    )
  )
)


(define (test_arctg x) ;функція що рахує вбудованими функціями arctg
  (cond
    ((>= 1)
     (/ (atan x) (atan (- x 5)))
      )
    ((and (<= 0 x) (< 1))
     (+ (atan x) (atan(* 2 x))))
    (else "Incorrect value")
    )
)


(define (iter_Talior i) ;Фунція що проходить ітерації значень [0, 3] для Ряду тейлора arctg
   (cond
    ((and (>= i 0) (<= i 3))
      (display i)
      (display " = ")
      (display (Tailor_series i)) 
      (newline)
      (iter (+ i 0.5))
    )
    (else 
      (display "")
    )
  )
)



(define (Tailor_series x) ;функція що рахує для Ряду тейлора arctg
  (cond
    ((>= 1)
     (/ (arctg_Tailor x 1) (arctg_Tailor (- x 5) 1)) 
      )
    ((and (<= 0 x) (< 1))
     (+ (arctg_Tailor x 1) (arctg_Tailor (* 2 x) 1))) 
    (else "Incorrect value")
    )
 )



(define (calc_arctg_Tailor x n); Розрахунок для кожної ітерації
  (* (expt -1 (- n 1)) (/ (expt x (- (* 2 n) 1)) (- (* 2 n) 1)))
 )
  


(define (arctg_Tailor x n); Функція що сумую Ряд тейлора
  (cond
    ((<= n 10)
     (+ x (calc_arctg_Tailor x (+ n 1)))
     )
   )
  )


(display "Built-in function")
(newline)
(iter 0)

(newline)
(newline)

(display "Tailor:")
(newline)
(iter_Talior 0)



(newline)
(display "---------Task 2---------")
(newline)


(define (last_iter i sum number) ;функція що шукає жружні числа
  (cond
    ((and (>= i 200) (<= i 300))
     (cond
       ( (and (= number (iter_div 1 i 0) ) (= sum i) )
         (display "(")
         (display number)
         (display ",sum: ")
         (display sum)
        (display ")")
        (display "(")
         (display i)
         (display ",sum: ")
         (display (iter_div 1 i 0))
        (display ")")
        (newline)
        )
       )
     
     (last_iter (+ i 1) sum number)
     )
    )
)
  

  

(define (iter i) ;Фунція що проходить ітерації значень [200, 300]
  (cond
    ((and (>= i 200) (<= i 300))
     (last_iter i (iter_div 1 i 0) i)
      (iter (+ i 1))
    )

      
  )
)


(define (iter_div i x current_sum) ;Фунція що вертає суму дільників числа і
  (cond
    ((and (>= i 1) (< i x))
     (cond
       ((= 0 (remainder x i))
        
       (iter_div (+ i 1) x (+ i current_sum))
       )
      (else (iter_div (+ i 1) x current_sum))
       )
    )
    (else 
      current_sum
    )
  )
)


(iter 200)




