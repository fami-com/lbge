(defpackage lbge.image-loader-test
  (:use :cl :rove))
(in-package lbge.image-loader-test)

(deftest make-image-test
  (ok (instance-of-p
          'lbge.image-loader::image
         (make-image :width 50 :height 50 :channels "rgba8" :data (vector 1 2 3 4)))))

(deftest load-image-test
  (ok (instance-of-p
          'lbge.image-loader::image
          (load-image "test-file.tga")))
  (testing "Error throws"
    (ok (signals (lbge.image-loader:load-image 1)))
    (ok (signals (lbge.image-loader:load-image 'a)))
    (ok (signals (lbge.image-loader:load-image '())))
    (ok (signals (lbge.image-loader:load-image '(a b c))))
    (ok (signals (lbge.image-loader:load-image :a)))
    (ok (signals (lbge.image-loader:load-image "test-file.png"))))) ;; While png isn't support


(defun instance-of-p (class obj)
  (eq class (type-of obj)))
