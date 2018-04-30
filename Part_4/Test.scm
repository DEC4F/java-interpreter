(load "Interpreter.scm")

(define test
  (lambda ()
    (display
     (if (string=? (string-append (test-4)) "")
         "\n All Tests Have Passed! \n\n"
         (string-append (test-4))))))

(define test-4
  (lambda ()
    (string-append
     (assert-equal "Test_4/Test_1.txt" 'A 15)
     (assert-equal "Test_4/Test_2.txt" 'A 12)
     (assert-equal "Test_4/Test_3.txt" 'A 125)
     (assert-equal "Test_4/Test_4.txt" 'A 36)
     (assert-equal "Test_4/Test_5.txt" 'A 54)
     (assert-equal "Test_4/Test_6.txt" 'A 110)
     (assert-equal "Test_4/Test_7.txt" 'C 26)
     (assert-equal "Test_4/Test_8.txt" 'Square 117)
     (assert-equal "Test_4/Test_9.txt" 'Square 32)
     (assert-equal "Test_4/Test_10.txt" 'List 15)
     (assert-equal "Test_4/Test_11.txt" 'List 123456)
     (assert-equal "Test_4/Test_12.txt" 'List 5285)
     ;(assert-equal "Test_4/Test_13.txt" 'C -716)
     )))

(define assert-equal
  (lambda (file class expected-value)
    (if (eq? (interpret file class) expected-value)
        ""
(string-append file " FAILED\n"))))
