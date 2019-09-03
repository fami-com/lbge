(defpackage :lbge.test.quaternion
  (:use :cl :lbge.math :rove))

(in-package :lbge.test.quaternion)

(deftest add-test
  (testing "quaternion addition"
    (ok (eqv (add (make-quaternion 1 2 3 4)
                  (make-quaternion 5 6 7 8))
             (make-float2 6 8 10 12)))))


(deftest sub-test
  (testing "quaternion subtraction"
    (ok (eqv (sub (make-quaternion 1 2 3 4)
                  (make-quaternion 5 6 7 8))
             (make-quaternion -4 -4 -4 -4)))))


(deftest mul-test
  (testing "quaternion by scalar multiplication"
    (ok (eqv (mul (make-quaternion 1 2 3 4) 2)
             (make-quaternion 2 4 6 8))))
  
  (testing "quaternion by quaternion multiplication"
    (ok (eqv (mul (make-quaternion 1 2 3 4)
                  (make-quaternion 5 6 7 8))
             (make-quaternion -60 12 30 24)))))


(deftest conj-test
  (testing "quaternion conjugate"
    (ok (eqv (conj (make-quaternion 1 2 3 4))
             (make-quaternion 1 -2 -3 -4)))))


(deftest inv-test
  (testing "quaternion inverse"
    (ok (eqv (inv (make-quaternion 1 2 3 4))
             (make-quaternion 1/30 -1/15 -1/10 -2/15)))))


(deftest norm-test
  (testing "quaternion norm"
    (ok (eqfp (norm (make-quaternion 1 2 3 4))
              (sqrt 30)))))


(deftest mul-test
  (testing "quaternion by scalar division"
    (ok (eqv (div (make-quaternion 2 4 6 8) 2)
             (make-quaternion 1 2 3 4)))))
 

(deftest negq-test
  (testing "quaternion negation"
    (ok (eqv (negq (make-quaternion 1 2 3 4))
             (make-quaternion -1 -2 -3 -4)))))


(deftest absq-test
  (testing "quaternion abs"
    (ok (eqv (absq (make-quaternion -1 -2 -3 -4))
             (make-quaternion 1 2 3 4)))))


(deftest normalize-test 
    (testing "quaternion normalize"
             ok (eqv (normalize (make-quaternion 1 2 3 4))
                     (make-quaternion (sqrt 1/30)
                                      (sqrt 1/15)
                                      (sqrt 1/10)
                                      (sqrt 2/15)))))
