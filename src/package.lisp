(in-package :defpackage+-user-1)

(defpackage+ :consmodel
  (:use #:cl #:alexandria)
  (:export-only

   ;; Material
   #:material #:material-values

   ;; Node
   #:node #:node-name #:node-objects #:node-transform #:node-children
   #:each-node

   ;; Mesh
   #:mesh #:mesh-id
   #:mesh-primitive-type #:mesh-vertices #:mesh-materials
   #:mesh-faces #:mesh-normals #:mesh-face-normals
   #:mesh-flat-vertex-count #:flatten-indexed #:expand-indexed

   ;; Camera
   #:camera
   #:camera-name #:camera-position #:camera-up #:camera-look-at
   #:camera-fov #:camera-aspect #:camera-clip-near #:camera-clip-far
   #:name #:position #:up #:fov #:aspect #:clip-near #:clip-far

   ;; Light
   #:light #:light-point #:light-directional #:light-spot
   #:light-name #:light-diffuse #:light-specular #:light-ambient
   #:light-position #:light-attenuation-constant #:light-attenuation-linear
   #:light-attenuation-quadratic #:light-direction

   ;; Model
   #:model #:model-cameras #:model-lights #:model-meshes #:model-materials
   #:model-stagings #:staging

   ;; Files
   #:convert-file #:read-cmdl-file
   )
  (:inherit-from #:kit.glm #:look-at))

(defpackage+ :consmodel.viewer
  (:use #:cl #:kit.glm #:kit.gl.shader #:kit.gl.vao #:consmodel)
  (:export #:view #:viewer))
