(in-package :lbge.math)

(defclass quaternion ()
  ((in-list
    :initarg :in-list
    :accessor in-list))
  (:default-initargs :in-list #(0 0 0 0)))

(defun make-quaternion (a1 &optional a2 a3 a4)
  (if (and a2 a3 a4)
      (make-instance 'quaternion :in-list (make-array '(4)
                                                  :initial-contents (list a1 a2 a3 a4)))
      (make-instance 'quaternion :in-list a1)))
