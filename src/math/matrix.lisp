(in-package :lbge.math)

(defclass floatnxn ()
  ((size :initarg :size
         :accessor matrix-size)
   (in-vec
    :initarg :in-vec
    :accessor in-vec)))

(defclass float2x2 (floatnxn)
  ((size :accessor matrix-size
         :initform 2) ; don't change this
   (in-vec
    :initarg :in-vec
    :accessor in-vec)))

(defclass float3x3 (floatnxn)
  ((size :accessor matrix-size
         :initform 3) ; don't change this
   (in-vec
    :initarg :in-vec
    :accessor in-vec)))

(defclass float4x4 (floatnxn)
  ((size :accessor matrix-size
         :initform 4) ; don't change this
   (in-vec
    :initarg :in-vec
    :accessor in-vec)))

(defmethod print-object ((mat float2x2) stream)
  (loop
    :for (a b) :on (coerce (in-vec mat) 'list)
    :by #'cddr
    :do (format stream "~A ~A~%" a b)))

(defmethod print-object ((mat float3x3) stream)
  (loop
    :for (a b c) :on (coerce (in-vec mat) 'list)
    :by #'cdddr
    :do (format stream "~A ~A ~A~%" a b c)))

(defmethod print-object ((mat float2x2) stream)
  (loop
    :for (a b) :on (coerce (in-vec mat) 'list)
    :by #'cddr
    :do (format stream "~A ~A~%" a b)))

(defmethod print-object ((mat float3x3) stream)
  (loop
    :for (a b c) :on (coerce (in-vec mat) 'list)
    :by #'cdddr
    :do (format stream "~A ~A ~A~%" a b c)))

(defmethod print-object ((mat float4x4) stream)
  (loop
    :for (a b c d) :on (coerce (in-vec mat) 'list)
    :by #'cddddr
    :do (format stream "~A ~A ~A ~A~%" a b c d)))

(defun make-float2x2 (&optional a00 a01 a10 a11)
  (cond ((null a00)
         (make-instance 'float2x2 :in-vec #(make-array '(4)
                                                       :initial-element 0.0f0)))
        ((and a00 a01 a10 a11)
         (make-instance 'float2x2
                        :in-vec (vector a00 a01
                                        a10 a11)))
        (a00
         (make-instance 'float2x2
                        :in-vec a00))))


(defun make-float3x3 (&optional a00 a01 a02 a10 a11 a12 a20 a21 a22)
  (cond ((null a00)
         (make-instance 'float3x3 :in-vec (make-array '(9)
                                                      :initial-element 0.0f0)))
        ((and a00 a01 a02 a10 a11 a12 a20 a21 a22)
         (make-instance 'float3x3
                        :in-vec (vector a00 a01 a02
                                        a10 a11 a12
                                        a20 a21 a22)))
        (a00
         (make-instance 'float3x3
                        :in-vec a00))))

(defun make-float4x4 (&optional a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33)
  (cond ((null a00)
         (make-instance 'float4x4 :in-vec (make-array '(16)
                                                      :initial-element 0.0f0)))
        ((and a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33)
         (make-instance 'float4x4
                      :in-vec (vector a00 a01 a02 a03
                                      a10 a11 a12 a13
                                      a20 a21 a22 a23
                                      a30 a31 a32 a33)))
        (a00
         (make-instance 'float4x4
                      :in-vec a00))))


(defun float2x2-zero () (make-float2x2))
(defun float3x3-zero () (make-float3x3))
(defun float4x4-zero () (make-float4x4))

(defun float2x2-one () (make-float2x2 1 1
                                      1 1))

(defun float3x3-one () (make-float3x3 1 1 1
                                      1 1 1
                                      1 1 1))

(defun float4x4-one () (make-float4x4 1 1 1 1
                                      1 1 1 1
                                      1 1 1 1
                                      1 1 1 1))


(defun float2x2-iden () (make-float2x2 1 0
                                       0 1))

(defun float3x3-iden () (make-float3x3 1 0 0
                                       0 1 0
                                       0 0 1))

(defun float4x4-iden () (make-float4x4 1 0 0 0
                                       0 1 0 0
                                       0 0 1 0
                                       0 0 0 1))


(defmacro get-at (matrix i j)
  `(aref (in-vec ,matrix)
        (+ ,j (* ,i (matrix-size ,matrix)))))

(defmacro set-at (matrix i j v)
  `(setf (aref (in-vec ,matrix)
          (+ ,j (* ,i (matrix-size ,matrix))))
         ,v))

(defmacro swap-vals (matrix i1 j1 i2 j2)
  `(rotatef (aref (in-vec ,matrix)
                 (+ ,j1 (* ,i1 (matrix-size ,matrix))))
            (aref (in-vec ,matrix)
                 (+ ,j2 (* ,i2 (matrix-size ,matrix))))))

(defmacro define-matrix-op (name result-type map-op)
  `(defmethod ,name ((matrix1 ,result-type) (matrix2 ,result-type))
     (make-instance ',result-type :in-vec
                    (map 'vector ,map-op
                         (in-vec matrix1)
                         (in-vec matrix2)))))

(defmacro define-matrix-num-op (name matrix-type map-fun)
  (flet ((body (func type)
              `(make-instance ',type :in-vec
                              (map 'vector ,func
                                           (in-vec matrix)))))
     `(progn
       (defmethod ,name ((value real) (matrix ,matrix-type))
         ,(body `(ax:curry ,map-fun value) matrix-type))
       (defmethod ,name ((matrix ,matrix-type) (value real))
         ,(body `(ax:rcurry ,map-fun value) matrix-type)))))

(defmacro define-matrix-num-op (name matrix-type map-fun)
  (flet ((body (func type)
              `(make-instance ',type :in-vec
                              (map 'vector ,func
                                           (in-vec matrix)))))
     `(progn
       (defmethod ,name ((value real) (matrix ,matrix-type))
         ,(body `(ax:curry ,map-fun value) matrix-type))
       (defmethod ,name ((matrix ,matrix-type) (value real))
         ,(body `(ax:rcurry ,map-fun value) matrix-type)))))

(defmacro define-matrix-unary-op (name matrix-type map-fun)
  `(defmethod ,name ((matrix ,matrix-type))
     (make-instance ',matrix-type :in-vec
                    (map 'vector ,map-fun
                         (in-vec matrix)))))


(define-matrix-op add float2x2 #'+)
(define-matrix-op add float3x3 #'+)
(define-matrix-op add float4x4 #'+)

(define-matrix-op sub float2x2 #'-)
(define-matrix-op sub float3x3 #'-)
(define-matrix-op sub float4x4 #'-)


(define-matrix-num-op mul float2x2 #'*)
(define-matrix-num-op mul float3x3 #'*)
(define-matrix-num-op mul float4x4 #'*)

(define-matrix-num-op div float2x2 #'/)
(define-matrix-num-op div float3x3 #'/)
(define-matrix-num-op div float4x4 #'/)


(defmacro get-row (matrix j)
  (let ((varname (gensym)))
      `(loop for ,varname from 0 below (matrix-size ,matrix)
             collect (get-at ,matrix ,varname ,j))))

(defmacro get-col (matrix i)
  (let ((varname (gensym)))
      `(loop for ,varname from 0 below (matrix-size ,matrix)
             collect (get-at ,matrix ,i ,varname))))


(defmethod mul ((matrix float2x2) (value float2x2))
  (let ((outm (float2x2-zero)))
    (dotimes (i 2)
      (dotimes (j 2)
       (set-at outm i j
               (reduce #'+
                       (mapcar #'*
                               (get-col matrix i)
                               (get-row value j))))))
    outm))

(defmethod mul ((matrix float3x3) (value float3x3))
  (let ((outm (float3x3-zero)))
    (dotimes (i 3)
      (dotimes (j 3)
        (set-at outm i j
                (reduce #'+
                        (mapcar #'*
                                (get-col matrix i)
                                (get-row value j))))))
    outm))

(defmethod mul ((matrix float4x4) (value float4x4))
  (let ((outm (float4x4-zero)))
    (dotimes (i 4)
      (dotimes (j 4)
        (set-at outm i j
                (reduce #'+
                        (mapcar #'*
                                (get-col matrix i)
                                (get-row value j))))))
    outm))


(defmethod mul ((matrix float2x2) (value float2))
  (make-float2
    (loop for i from 0 to 1 sum
          (* (get-at matrix 0 i)
             (x value)))
    (loop for i from 0 to 1 sum
          (* (get-at matrix 1 i)
             (y value)))))

(defmethod mul ((matrix float3x3) (value float3))
  (make-float3
    (loop for i from 0 to 2 sum
          (* (get-at matrix 0 i)
             (x value)))
    (loop for i from 0 to 2 sum
          (* (get-at matrix 1 i)
             (y value)))
    (loop for i from 0 to 2 sum
          (* (get-at matrix 2 i)
             (z value)))))

(defmethod mul ((matrix float4x4) (value float4))
  (make-float4
    (loop for i from 0 to 3 sum
          (* (get-at matrix 0 i)
             (x value)))
    (loop for i from 0 to 3 sum
          (* (get-at matrix 1 i)
             (y value)))
    (loop for i from 0 to 3 sum
          (* (get-at matrix 2 i)
             (z value)))
    (loop for i from 0 to 3 sum
          (* (get-at matrix 3 i)
             (w value)))))


(defmacro det2x2 (a11 a12 a21 a22)
  `(- (* ,a11 ,a22)
      (* ,a12 ,a21)))

(defmacro det3x3 (a11 a12 a13 a21 a22 a23 a31 a32 a33)
  `(- (+ (* ,a11
            (det2x2 ,a22 ,a23
                    ,a32 ,a33))
         (* ,a13
            (det2x2 ,a21 ,a22
                    ,a31 ,a32)))
      (* ,a12
         (det2x2 ,a21 ,a23
                 ,a31 ,a33))))

(defmacro det4x4 (a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44)
  `(- (+ (* ,a11
            (det3x3 ,a22 ,a23 ,a24
                    ,a32 ,a33 ,a34
                    ,a42 ,a43 ,a44))
         (* ,a13
            (det3x3 ,a21 ,a22 ,a24
                    ,a31 ,a32 ,a34
                    ,a41 ,a42 ,a44)))
      (+ (* ,a12
            (det3x3 ,a21 ,a23 ,a24
                    ,a31 ,a33 ,a34
                    ,a41 ,a43 ,a44))
         (* ,a14
            (det3x3 ,a21 ,a22 ,a23
                    ,a31 ,a32 ,a33
                    ,a41 ,a42 ,a43)))))

(defmethod det ((matrix floatnxn))
  (let* ((size (matrix-size matrix))
         (data (coerce (in-vec matrix) 'list)))
    (eval (ecase size
           ((2) `(det2x2 ,@data))
           ((3) `(det3x3 ,@data))
           ((4) `(det4x4 ,@data))))))

(define-matrix-unary-op absm float2x2 #'abs)
(define-matrix-unary-op absm float3x3 #'abs)
(define-matrix-unary-op absm float4x4 #'abs)

(define-matrix-unary-op negm float2x2 #'-)
(define-matrix-unary-op negm float3x3 #'-)
(define-matrix-unary-op negm float4x4 #'-)

(defmethod transpose ((*matrix* floatnxn))
  (declare (special *matrix*))
  (let* ((size (matrix-size *matrix*))
         (data (coerce (mapcar #'eval
                               (genfuncs 'get-at size size
                                         :before '(*matrix*)
                                         :swapped t))
                       'vector)))
    (ecase size
     ((2) (make-float2x2 data))
     ((3) (make-float3x3 data))
     ((4) (make-float4x4 data)))))

(defun eqm (matrix1 matrix2 &optional (eps +epsilon+))
  (reduce #'hand
          (map 'vector (ax:rcurry #'eqfp eps)
               (in-vec matrix1)
               (in-vec matrix2))))

(defun neqm (matrix1 matrix2 &optional (eps +epsilon+))
  (not (eqm matrix1 matrix2 eps)))


(defun make-ortho-projection (&key left right top bottom near far)
  (let ((l left)
        (r right)
        (tp top)
        (b bottom)
        (n near)
        (f far))
    (make-float4x4 (/ 2 (- r l)) 0 0 (- (/ (+ r l) (- r l)))
                   0 (/ 2 (- tp b)) 0 (- (/ (+ tp b) (- tp b)))
                   0 0 (/ -2 (- n f)) (- (/ (+ f n) (- f n)))
                   0 0 0 1)))

(defun make-look-at (position target up)
  (let* ((dir (normalize (sub target position)))
         (right (normalize (cross dir up)))
         (norm-up (normalize (cross right dir))))
    (make-float4x4
     (float4-x right)
     (float4-y right)
     (float4-z right)
     (dot right (negv position))

     (float4-x norm-up)
     (float4-y norm-up)
     (float4-z norm-up)
     (dot norm-up (negv position))

     (float4-x dir)
     (float4-y dir)
     (float4-z dir)
     (dot dir (negv position))

     0.0
     0.0
     0.0
     1.0)))
