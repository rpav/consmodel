(in-package :consmodel)

(defun convert-file (input-file &key output-file)
  (unless (probe-file input-file)
    (error "File not found: ~S" input-file))
  (let ((input (without-warnings ()     ; Damn CFFI.
                 (ai:import-into-lisp
                  input-file
                  :processing-flags '(:ai-process-join-identical-vertices
                                      :ai-process-triangulate
                                      :ai-process-find-degenerates
                                      :ai-process-find-invalid-data
                                      :ai-process-sort-by-p-type
                                      :ai-process-optimize-meshes))))
        (output))
    (unwind-protect
         (progn
           (when output-file
             (setf output (open output-file
                                :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))))
           (fast-io:with-fast-output (buffer output)
             (let* ((scene (make-node input))
                    (packed (conspack:tracking-refs ()
                              (conspack:with-named-index 'model-index-1
                                (conspack:encode scene)))))
               (fast-io:fast-write-sequence
                (conspack:encode (list "CMDL" 1 0 0))
                buffer)
               (fast-io:fast-write-sequence packed buffer))))
      (when output (close output)))))

(defun read-cmdl-file (pathname)
  (conspack:with-named-index 'model-index-1
    (conspack:tracking-refs ()
      (cadr (conspack:decode-file pathname)))))
