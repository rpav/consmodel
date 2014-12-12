(in-package :consmodel)

(conspack:define-index model-index-1
  ;; Most-common symbols
  name vertices materials transform children
  primitive-type :point :line :triangle
  material material-simple

  consmodel model mesh node stage

  ;; Slightly less-common
  camera light light-point light-directional

  ;; Common material symbols
  :alpha :ambient :diffuse :roughness :specular :specular-hardness
  :specular-intensity :specular-ior

  ;; Camera
  position up look-at fov aspect clip-near clip-far

  ;; Light
  diffuse specular attenuation-constant attenuation-linear
  attenuation-quadratic direction

  ;; Model
  stagings)

(defclass named ()
  ((name :initform nil :initarg :name :accessor name)))

(defmethod print-object ((o named) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (with-slots (name) o
      (format stream "~S" name))))

(defclass node (named)
  ((transform :initform nil :initarg :transform :accessor node-transform)
   (children :initform nil :initarg :children :accessor node-children)))

(defclass mesh (node)
  ((primitive-type :initform :triangle :initarg :primitive-type
                   :accessor mesh-primitive-type)
   (vertices :initform nil :initarg :vertices :accessor mesh-vertices)
   (normals :initform nil :initarg :normals :accessor mesh-normals)
   (faces :initform nil :initarg :faces :accessor mesh-faces)
   (face-normals :initform nil :initarg :face-normals :accessor mesh-face-normals)
   (materials :initform nil :initarg :materials :accessor mesh-materials)))

(defclass camera (node)
  ((position :initarg :position :accessor camera-position)
   (up :initarg :up :accessor camera-up)
   (look-at :initarg :look-at :accessor camera-look-at)
   (fov :initarg :fov :accessor camera-fov)
   (aspect :initarg :aspect :accessor camera-aspect)
   (clip-near :initarg :clip-near :accessor camera-clip-near)
   (clip-far :initarg :clip-far :accessor camera-clip-far)))

(defclass light (node)
  ((diffuse :initarg :diffuse :accessor light-diffuse)
   (specular :initarg :specular :accessor light-specular)))

(defclass light-point (light)
  ((position :initarg :position :accessor light-position)
   (attenuation-constant :initarg :attenuation-constant :accessor light-attenuation-constant)
   (attenuation-linear :initarg :attenuation-linear :accessor light-attenuation-linear)
   (attenuation-quadratic :initarg :attenuation-quadratic :accessor light-attenuation-quadratic)))

(defclass light-directional (light)
  ((direction :initarg :direction :accessor light-direction)))

(defclass model ()
  ((stagings :initform nil :initarg :stagings :accessor model-stagings)))

(defun staging (model n)
  (with-slots (stagings) model
    (aref stagings n)))

(defclass material (named)
  ((name :initform "" :reader material-name :initarg :name)))

(defclass material-simple (material)
  ((values :initform (make-hash-table) :initarg :values :reader material-values)))

(defgeneric make-node (source &key &allow-other-keys))

(defmethod make-node ((ai-scene ai:scene) &key &allow-other-keys)
  (let* ((ai-meshes (ai:meshes ai-scene))
         (ai-mats (ai:materials ai-scene))
         #++(ai-anims (ai:animations ai-scene))
         (ai-cams (ai:cameras ai-scene))
         (ai-lights (ai:lights ai-scene))
         (new-meshes (make-array (length ai-meshes)))
         (new-materials (make-array (length ai-mats)))
         (name-map (make-hash-table :test 'equal)))
    (loop for old-material across ai-mats
          for i from 0
          as material = (make-hash-table)
          do (maphash (lambda (k v)
                        (let ((new-key (ai-mat-to-symbol k)))
                          (setf (gethash new-key material) v)))
                      old-material)
             (let ((material-name (gethash :material-name material)))
               (remhash :material-name material)
               (setf (aref new-materials i)
                     (make-instance 'material-simple
                       :name material-name
                       :values material))))
    (loop for i from 0
          for mesh across ai-meshes
          do (setf (aref new-meshes i) (make-node mesh :materials new-materials)))
    (map 'null (lambda (x)
                 (setf (gethash (getf (cdr x) :name) name-map)
                       (make-light x)))
         ai-lights)
    (map 'null (lambda (x)
                 (setf (gethash (car x) name-map)
                       (make-camera x)))
         ai-cams)
    (make-instance 'model
      :stagings (vector (make-node (ai:root-node ai-scene)
                                   :objects name-map
                                   :meshes new-meshes)))))

(defun make-camera (c)
  (destructuring-bind (name &key position up look-at horizontal-fov clip-near clip-far aspect)
      c
    (make-instance 'camera
      :name name
      :position position
      :up up
      :look-at look-at
      :fov horizontal-fov
      :aspect aspect
      :clip-near clip-near
      :clip-far clip-far)))

(defun make-light (light)
  (destructuring-bind (type &key name position diffuse specular ambient
                       attenuation-constant attenuation-linear attenuation-quadratic
                       direction inner-angle outer-angle)
      light
    (declare (ignore ambient inner-angle outer-angle))
    (ecase type
      (:ai-light-source-point
       (make-instance 'light-point
         :name name
         :position position
         :diffuse diffuse
         :specular specular
         :attenuation-constant attenuation-constant
         :attenuation-linear attenuation-linear
         :attenuation-quadratic attenuation-quadratic))
      (:ai-light-source-directional
       (make-instance 'light-directional
         :name name
         :direction direction
         :diffuse diffuse
         :specular specular)))))

(defmethod make-node ((ai-node ai:node) &key objects meshes &allow-other-keys)
  (let ((tfm (kit.glm:transpose-matrix
              (make-array 16 :element-type 'single-float
                             :initial-contents (ai:transform ai-node))))
        (children
          (concatenate 'vector
            (map 'vector (lambda (x) (aref meshes x)) (ai:meshes ai-node))
            (map 'vector
                 (lambda (x)
                   (make-node x :objects objects :meshes meshes))
                 (ai:children ai-node)))))
    (if-let ((node (gethash (ai:name ai-node) objects)))
      (progn
        (setf (node-transform node) tfm)
        (setf (node-children node) children)
        node)
      (make-instance 'node
        :name (ai:name ai-node)
        :transform tfm
        :children children))))

(defun set-vertex-face (vector n offset vertices faces)
  "=> FACE-VERTICES
Copy the vertices for the Nth face into `VECTOR` at `OFFSET`.  Returns
the number of vertices in the face for tracking."
  (let* ((total-vertices (length (elt faces n))))
    (loop for vertex-index across (elt faces n)
          for i from 0
          as vertex = (aref vertices vertex-index)
          do (loop for j from 0 below 3
                   do (setf (aref vector (+ offset (* i 3) j))
                            (aref vertex j))))
    total-vertices))

(defun set-attr3 (vector offset face attr)
  (loop for index across face
        for i from offset by 3
        as vertex = (aref attr index)
        do (setf (aref vector (+ 0 i)) (aref vertex 0))
           (setf (aref vector (+ 1 i)) (aref vertex 1))
           (setf (aref vector (+ 2 i)) (aref vertex 2))))

(defun expand-indexed (index dest data &optional (n 3))
  "Expand indexed data in `INDEX` to `DEST`, where each element refers
to a set of elements in `DATA`.  `DATA` should be a flat array of `N
* (LENGTH INDEX)` elements. `DEST` must be preallocated to `N
* (LENGTH INDEX)`."
  (loop for x across index
        for i from 0 by n
        as n*x = (* n x)
        do (replace dest data
                    :start1 i
                    :start2 n*x
                    :end2 (+ n n*x))))

(defun flatten-array (array type)
  (let ((v (make-array (* 3 (length array)) :element-type type)))
    (loop for sub across array
          for i from 0
          do (replace v sub :start1 (* i 3)))
    v))

(defun best-integer-type (n)
  (cond
    ((< n (expt 2 8))  '(unsigned-byte 8))
    ((< n (expt 2 16)) '(unsigned-byte 16))
    ((< n (expt 2 32)) '(unsigned-byte 32))
    (t t)))

(defun mesh-flat-vertex-count (mesh)
  (length (mesh-faces mesh)))

(defun ai-mat-to-symbol (string)
  (let* ((dot (position #\. string))
         (prefix (substr string 1 4))
         (type (cond
                 ((string= prefix "mat") "MATERIAL-")
                 ((string= prefix "clr") "")
                 (t (string-upcase prefix))))
         (name (string+ type (string-upcase (substr string (1+ dot))))))
    (make-keyword name)))

(defmethod make-node ((mesh ai:mesh) &key materials)
  (let ((primitive-type
          (cond
            ((ai:mesh-has-points mesh) :point)
            ((ai:mesh-has-lines mesh) :line)
            ((ai:mesh-has-triangles mesh) :triangle)
            (:polygon)))
        (material (aref materials (ai:material-index mesh)))
        (flat-faces (flatten-array (ai:faces mesh) (best-integer-type (length (ai:faces mesh)))))
        (flat-vertices (flatten-array (ai:vertices mesh) 'single-float))
        (flat-normals (flatten-array (ai:normals mesh) 'single-float)))
    (make-instance 'mesh
      :primitive-type primitive-type
      :vertices flat-vertices
      :normals flat-normals
      :faces flat-faces
      :face-normals flat-faces
      :materials (vector material))))

(defvar *transform* (kit.glm:identity-matrix))

(defgeneric each-node (function node &key transform-p transform filter &allow-other-keys)
  (:documentation "Call `FUNCTION` on `NODE` and every child of `NODE`,
by default passing the transformation matrix.

If `:transform is specified, this is used as a base transformation.
If `:transform-p is nil, no transformations are done.  If `:filter` is
non-nil, it should be a type specifier; `FUNCTION` will only be called
on objects which are `typep` this specifier."))

(defmethod each-node (function (node node)
                      &key (transform-p t) (transform kit.glm:+identity-matrix+)
                      filter
                      &allow-other-keys)
  (with-slots ((tfm transform) children) node
    (let ((tfm (when transform-p
                 (if tfm
                     (kit.glm:matrix* transform tfm)
                     transform))))
      (when (or (not filter) (typep node filter))
        (if transform-p
            (funcall function node tfm)
            (funcall function node)))
      (when (vectorp children)
        (loop
          for child across children
          do (each-node function child
                        :filter filter
                        :transform-p transform-p
                        :transform (or tfm kit.glm:+identity-matrix+)))))))

(conspack:defencoding model
  stagings)

(conspack:defencoding node
  name transform children)

(conspack:defencoding mesh
  name transform children
  primitive-type vertices normals faces face-normals materials)

(conspack:defencoding camera
  name transform children
  position up look-at fov aspect clip-near clip-far)

(conspack:defencoding light-point
  name transform children
  position diffuse specular
  attenuation-constant attenuation-linear attenuation-quadratic)

(conspack:defencoding light-directional
  name transform children
  position diffuse specular direction)

(conspack:defencoding material-simple
  name values)
