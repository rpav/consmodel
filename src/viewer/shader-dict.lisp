(in-package :consmodel.viewer)

(defdict shaders-3.3 (:shader-path (asdf-path :consmodel "viewer" "shaders"))
  (shader simple-color-vs :vertex-shader (:file "simple-vs.glsl"))
  (shader simple-color-fs :vertex-shader (:file "simple-fs.glsl"))
  (program :color (:mvp-m :mv-m :normal-m :num-lights)
           (:vertex-shader simple-color-vs)
           (:fragment-shader simple-color-fs)))

(defvao simple-flat ()
  (:separate ()
    (vertex :float 3)
    (normal :float 3)))

(defvao simple-color ()
  (:separate ()
    (vertex :float 3)
    (normal :float 3)
    (color :float 3)))
