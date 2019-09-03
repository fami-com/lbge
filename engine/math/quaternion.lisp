(in-package :lbge.math)

(defclass quaternion ()
  ((in-list
    :initarg :in-list
    :accessor in-list))
  (:default-initargs :in-list #(0 0 0 0)))


(defgeneric add (quat1 quat2)
  :documentation "Add two quaternions")

(defgeneric sub (quat1 quat2)
  :documentation "Subtract quat2 from quat1")

(defgeneric mul (quat1 quat2)
  :documentation "Multiply two quaternions or a quaternion and a scalar")

(defgeneric div (quat value)
  :documentation "Divide each element of quat by a scalar")

(defgeneric conj (quat)
  :documentation "Get the conjugate of a quaternion")

(defgeneric norm (quat)
  :documentation "Get the norm of a quaternion")

(defgeneric normalize (quat)
  :documentation "Get a normalized form (verser) of a quaternion")

(defgeneric inv (quat)
  :documentation "Get the inverse of a quaternion")

(defgeneric absq (quat)
  :documentation "Get the absolute value of a quaternion")

(defgeneric negq (quat)
  :documentation "Negate a quaternion")

(defgeneric eqq (quat1 quat2)
  :documentation "Check whether quaternions are equal")

(defgeneric neqq (quat1 quat2)
  :documentation "Check whether quaternions are not equal")

(defgeneric to-euler-angles (quat)
  :documentation "Transform a quaternion to euler angles")

(defun w (quat)
  (aref (in-list quat) 0))

(defun x (quat)
  (aref (in-list quat) 1))

(defun y (quat)
  (aref (in-list quat) 2))

(defun z (quat)
  (aref (in-list quat) 3))


(defun make-quaternion (a1 &optional a2 a3 a4)
  (if (and a2 a3 a4)
      (make-instance 'quaternion :in-list (make-array '(4)
                                                  :initial-contents (list a1 a2 a3 a4)))
      (make-instance 'quaternion :in-list a1)))


(defmethod add ((quat1 quaternion) (quat2 quaternion))
  (make-quaternion
   (map 'vector #'+
        (in-list quat1)
        (in-list quat2))))


(defmethod sub ((quat1 quaternion) (quat2 quaternion))
  (make-quaternion
   (map 'vector #'-
        (in-list quat1)
        (in-list quat2))))


(defmethod mul ((quat1 quaternion) (quat2 quaternion))
  (make-quaternion
   (- (* (w quat1) (w quat2))
      (* (x quat1) (x quat2))
      (* (y quat1) (y quat2))
      (* (z quat1) (z quat2))) ; the w part
   (- (+ (* (w quat1) (x quat2))
         (* (x quat1) (w quat2))
         (* (y quat1) (z quat2)))
      (* (z quat1) (y quat2))) ; the x part
   (- (* (w quat1) (y quat2))
      (+ (* (x quat1) (z quat2))
         (* (y quat1) (w quat2))
         (* (z quat1) (x quat2)))) ; the y part
   (- (+ (* (w quat1) (z quat2))
         (* (x quat1) (y quat2)))
      (+ (* (y quat1) (x quat2))
         (* (z quat1) (w quat2)))))) ; the z part


(defmethod mul ((quat1 quaternion) (quat2 real))
  (make-quaternion
   (map 'vector
        (lambda (x)
          (* x quat2))
        (in-list quat1))))

(defmethod div ((quat1 quaternion) (quat2 real))
  (make-quaternion
   (map 'vector
        (lambda (x)
          (/ x quat2))
        (in-list quat1))))


(defmethod conj ((quat quaternion))
  (make-quaternion
   (w quat)
   (- (x quat))
   (- (y quat))
   (- (z quat))))


(defmethod norm ((quat quaternion))
  (sqrt
   (reduce #'+
           (map 'list
                (lambda (x)
                  (expt x 2))
                (in-list quat)))))


(defmethod normalize ((quat quaternion))
  (div quat (norm quat)))


(defmethod inv ((quat quaternion))
  (div (conj quat) (expt (norm quat) 2)))


(defmethod absq ((quat quaternion))
  (make-quaternion
   (map 'vector #'abs
        (in-list quat))))


(defmethod negq ((quat quaternion))
  (make-quaternion
   (map 'vector #'-
        (in-list quat))))

(defmethod to-euler-angles ((quat quaternion))
  (make-float3
   (atan (/ (* 2 (+ (* (w quat) (x quat))
                    (* (y quat) (z quat))))
            (- 1 (* 2 (+ (expt (x quat) 2)
                         (expt (y quat) 2)))))) ; roll, phi
   (asin (* 2 (- (* (w quat) (y quat))
                 (* (x quat) (z quat))))) ; pitch, theta
   (atan (/ (* 2 (+ (* (w quat) (z quat))
                    (* (x quat) (y quat))))
            (- 1 (* 2 (+ (expt (x quat) 2)
                         (expt (y quat) 2)))))))) ; yaw, psi

(defun from-euler-angles (vec3)
  (with-slots ((phi x) (theta y) (psi z)) vec3
    (make-quaternion
     (+ (* (cos (/ phi 2))
           (cos (/ theta 2))
           (cos (/ psi 2)))
        (* (sin (/ phi 2))
           (sin (/ theta 2))
           (sin (/ psi 2)))) ; w part (real)
     (- (* (sin (/ phi 2))
           (cos (/ theta 2))
           (cos (/ psi 2)))
        (* (cos (/ phi 2))
           (sin (/ theta 2))
           (sin (/ psi 2)))) ; x part (i)
     (+ (* (cos (/ phi 2))
           (sin (/ theta 2))
           (cos (/ psi 2)))
        (* (sin (/ phi 2))
           (cos (/ theta 2))
           (sin (/ psi 2)))) ; y part (j)
     (- (* (cos (/ phi 2))
           (cos (/ theta 2))
           (sin (/ psi 2)))
        (* (sin (/ phi 2))
           (sin (/ theta 2))
           (cos (/ psi 2))))))) ; z part (k)


(defmethod eqq ((quat1 quaternion) (quat2 quaternion))
  (reduce #'hand
          (map 'list #'eqfp
               (in-list quat1)
               (in-list quat2))))

(defmethod neqq ((quat1 quaternion) (quat2 quaternion))
  (not (eqq quat1 quat2))


(defun quat-zero () (make-quaternion 0 0 0 0))
(defun quat-one () (make-quaternion 1 1 1 1))
(defun quat-w () (make-quaternion 1 0 0 0))
(defun quat-x () (make-quaternion 0 1 0 0))
(defun quat-y () (make-quaternion 0 0 1 0))
(defun quat-z () (make-quaternion 0 0 0 1))
