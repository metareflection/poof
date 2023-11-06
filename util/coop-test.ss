(import
  :std/debug/DBG
  :std/test
  "coop.ss")

(export coop-test)

(def coop-test
  (test-suite "test coop"
    (test-case "simple records"
      (def a (@ rcons 'x 3 (@ rcons 'y 4 top)))
      (check (list (a 'x) (a 'y)) => '(3 4))
      (check-exception (a 'z) true)
      (def b (make-record 'x 8 'y 9))
      (check (list (b 'x) (b 'y)) => '(8 9))
      (check-exception (b 'z) true)
      (def c (make-record 'z 5 't 6 a))
      (check (list (c 'x) (c 'y) (c 'z) (c 't)) => '(3 4 5 6))
      (check-exception (c 'u) true))
    (test-case "simple record prototypes"
      (def point1 (rfix (@ $method/const 'x 3)))
      (check (point1 'x) => 3)
      (define point2 (rfix (@ $method/const 'x 3) (@ $kv 'y 4)))
      (check (map point2 '(x y)) => '(3 4))
      (check (map (rfix ($record x 1 y 2 z 3)) '(x y z)) => '(1 2 3))
      (check (map (@ fix ($methods 'x 1 'y 2 'z 3) top) '(x y z)) => '(1 2 3)))
    (test-case "inheritance tests"
      (def point1 (rfix (mix*
                         (@ $method/next 'y (λ (next) (+ 10 (next '_))))
                         (@ $kv 'x 1) ($record x 2 y 3))))
      (check (map point1 '(x y)) => '(1 13))
      (def point2 (rfix (rmix*
                         (@ $method/self 'z (λ (self) (+ (self 'y) (self 'x))))
                         (@ $kv 'x 1) ($record x 2 y 3))))
      (check (map point2 '(x y z)) => '(2 3 5))
      )))
