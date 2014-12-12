(in-package :consmodel.viewer)

(defclass renderer ()
  ((programs :initform nil)))

(defgeneric activate (renderer object)
  (:documentation "Activate or bind GL objects such as textures, buffers,
and programs.  Specialize on `RENDERER` and `OBJECT`.  Specialization
on `OBJECT` as `SYMBOL` is reserved for activating programs by name."))

(defmethod activate ((r renderer) (object symbol))
  "Activate program named `SYMBOL`."
  (with-slots (programs) r
    (kit.gl.shader:use-program programs object)))

 ;; GL 3.3

(defclass renderer-3.3 (renderer) ())

(defmethod initialize-instance ((r renderer-3.3) &key &allow-other-keys)
  (with-slots (programs) r
    (gl:enable :depth-test)
    (setf programs
          (kit.gl.shader:compile-shader-dictionary 'shaders-3.3))))
