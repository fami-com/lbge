(in-package :lbge.math)

(defclass newton-interp ()
  ((divdiffs :initarg :divdiffs :accessor divdiffs)
   (xs :initarg :xs :accessor xs)
   (poly :initarg :poly :accessor poly)))

(defmethod degree ((interp newton-interp))
  (length (xs interp)))

(defun div-diff (xs row start order)
  (let ((y0 (nth (1+ start) row))
        (y1 (nth start row))
        (x0 (nth (+ start order) xs))
        (x1 (nth start xs)))
      (/ (- y0 y1)
         (- x0 x1))))

(defun calc-div-diffs (xs ys)
  (let ((diffs (list ys))
        (len (length ys)))
     (loop for o from 1 below len
      do (append-to diffs
                    (loop for n from 0 below (- len o)
                          collect (div-diff xs (nth (1- o) diffs) n o))))
     diffs))

(defun calc-basis-polynomial (interp order)
  (let ((p (make-polynomial 1)))
       (loop for x in (subseq (xs interp) 0 order)
             do (setf p (mul p (make-polynomial (- x) 1))))
      p))

(defun calc-newton-polynomial (interp)
  (let ((p (make-polynomial 0)))
      (loop for i from 0 below (degree interp)
            do (setf p (add p
                            (mul (calc-basis-polynomial interp i)
                                 (first (nth i (divdiffs interp)))))))
      p))

(defun make-newton-raw (coords)
  (assert (>= (length coords) 3)
          nil
          "There must be at least 3 points to interpolate")
  (loop for c in coords
        collect (float2-x c) into xs
        collect (float2-y c) into ys
        finally (return (make-instance 'newton-interp
                                       :xs xs
                                       :divdiffs (calc-div-diffs xs ys)))))

(defun make-newton (coords)
  (let ((newt (make-newton-raw coords)))
      (setf (poly newt) (calc-newton-polynomial newt))
      newt))

(defun copy-newton-interp (interp)
  (with-slots (xs divdiffs poly) interp
   (make-instance 'newton-interp
    :xs (copy-list xs)
    :divdiffs (copy-tree divdiffs)
    :poly (make-polynomial (ax:copy-array (coeffs poly))))))

(defmethod nadd-point ((interp newton-interp) (point float2))
  (let ((x (float2-x point))
        (y (float2-y point)))
    (append-to (xs interp) x)
    (append-to (first (divdiffs interp)) y)
    (append-to (divdiffs interp) (list))
    (loop for o from 1 below (degree interp)
          do (append-to (nth o (divdiffs interp))
                        (div-diff (xs interp)
                                  (nth (1- o) (divdiffs interp))
                                  (- (degree interp) o 1)
                                  o)))
    (setf (poly interp) (add (poly interp) (mul (calc-basis-polynomial interp
                                                                       (1- (degree interp)))
                                                (caar (last (divdiffs interp))))))
    interp))

(defmethod add-point ((interp newton-interp) (point float2))
  (nadd-point (copy-newton-interp interp) point))

(defmethod nadd-points ((interp newton-interp) (points list))
  (loop for p in points
        do (nadd-point interp p)
        return interp))

(defmethod add-points ((interp newton-interp) (points list))
  (loop with interpc = (copy-newton-interp interp)
        for p in points
        do (nadd-point interpc p)
        return interp))

(defmethod call-at ((interp newton-interp) (x real))
  (call-at (poly interp) x))
