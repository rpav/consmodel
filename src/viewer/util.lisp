(in-package :consmodel.viewer)

(defun each-recursively (function sequence &rest sequences)
  (typecase sequence
    (list (loop for i in sequence do (each-recursively function i)))
    (vector (loop for i across sequence do (each-recursively function i)))
    (t
     (funcall function sequence)
     (when (car sequences)
       (apply #'each-recursively (car sequences) (cdr sequences))))))

(defun nth-recursively (index seq)
  (let ((n 0))
    (block nil
      (each-recursively (lambda (x)
                          (when (= n index)
                            (return x))
                          (incf n))
                        seq))))

(defun minmax (vectors)
  (let* ((first (nth-recursively 0 vectors))
         (min first)
         (max first))
    (each-recursively
     (lambda (x)
       (setf min (min min x)
             max (max max x)))
     vectors)
    (values min max)))

(defun version>= (version-a version-b)
  (map nil (lambda (a b)
             (cond
               ((> a b) (return-from version>= t))
               ((< a b) (return-from version>= nil))))
       version-a version-b)
  t)

(defmacro version-case ((version &key (test ''version>=)) &body version-clauses)
  (alexandria:once-only (version test)
    `(cond ,@(loop for i in version-clauses collect
                (if (eq (car i) 'otherwise)
                    `(t ,@(cdr i))
                    `((funcall ,test ,version ',(car i)) ,@(cdr i)))))))

(defun gl-version ()
  (list (gl:get-integer :major-version)
        (gl:get-integer :minor-version)))

(defun asdf-path (system &rest path)
  (asdf:component-pathname
   (or (asdf:find-component (asdf:find-system system t) path)
       (error "System ~S path not found: ~S" system path))))
