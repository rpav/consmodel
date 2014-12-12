(defpackage :consmodel.asdf
  (:use #:cl #:asdf))

(in-package :consmodel.asdf)

(defsystem :consmodel
  :description "Convert assets via CLASSIMP to the conspack \"consmodel\" format"
  :author "Ryan Pavlik"
  :license "GPL"
  :version "0.0"

  :depends-on (:alexandria :defpackage-plus :cl-conspack
               :sdl2kit :glkit :classimp)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "util")
   (:file "model")
   (:file "format")

   (:module "viewer"
    :pathname "viewer"
    :serial t
    :components
    ((:module "shaders"
      :pathname "shaders"
      :components
      ((:static-file "simple-vs.glsl")
       (:static-file "simple-tex-vs.glsl")))

     (:file "util")
     (:file "shader-dict")
     (:file "viewer")))))
