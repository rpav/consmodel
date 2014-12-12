(in-package :consmodel.viewer)

(defparameter *default-material*
  (alexandria:alist-hash-table
   `((:diffuse . ,(vec3 0.7 0.7 0.7))
     (:specular . ,(vec3 0.8 0.8 0.8))
     (:specular-ior . 4.0)
     (:roughness . 0.2))))

(defclass viewer (sdl2.kit:gl-window)
  ((stage :reader viewer-stage)
   programs vaos cameras lights
   view-matrix projection-matrix))

(defun view (filename &key (w 1280) (h 720))
  (sdl2.kit:start)
  (make-instance 'viewer :file filename :w w :h h))

(defun setup-viewport (w h)
  (gl:viewport 0 0 w h))

(defun setup-matrices (viewer w h)
  (with-slots (stage cameras) viewer
    (if (> (length cameras) 0)
        (setup-matrices-camera viewer (aref cameras 0))
        (setup-matrices-guess viewer w h))))

(defun setup-matrices-camera (viewer camera)
  (with-slots (view-matrix projection-matrix mvp-matrix)
      viewer
    (with-slots (position fov aspect clip-near clip-far)
        (car camera)
      (setf view-matrix (kit.glm:inverse-matrix (cdr camera)))
      (setf projection-matrix
            (kit.glm:perspective-matrix fov aspect clip-near clip-far))
      (%gl:depth-range clip-near clip-far))))

(defun setup-matrices-guess (viewer w h)
  (with-slots (stage view-matrix projection-matrix mvp-matrix)
      viewer
    (multiple-value-bind (min max)
        (minmax (map 'list #'mesh-vertices (filter-stage stage 'mesh)))
      (setf view-matrix
            (kit.glm:look-at (vector (* 2 min) (* 2 min) (* 2 min))
                             #(0.0 0.0 0.0)
                             #(0.0 0.0 -1.0)))
      (setf projection-matrix
            (kit.glm:perspective-matrix 50.0 (/ w h) min max)))))

(defun filter-stage (stage type &key transforms-p)
  (let (list)
    (each-node (if transforms-p
                   (lambda (n tfm)
                     (push (cons n tfm) list))
                   (lambda (n tfm)
                     (declare (ignore tfm))
                     (push n list)))
               stage
               :filter type)
    (map 'vector #'identity
         (sort list #'string<
               :key (if transforms-p
                        (lambda (x) (name (car x)))
                        (lambda (x) (name x)))))))

(defmethod initialize-instance :before ((v viewer) &key &allow-other-keys)
  (sdl2:gl-set-attrs :context-major-version 3
                     :context-minor-version 3
                     :context-profile-mask 0))

(defmethod initialize-instance :after ((v viewer) &key file &allow-other-keys)
  (with-slots (programs stage cameras lights vaos) v
    (when file (setf stage (staging (read-cmdl-file file) 0)))
    (setf cameras (filter-stage stage 'camera :transforms-p t))
    (setf lights (filter-stage stage 'light :transforms-p t))

    (multiple-value-bind (w h) (kit.sdl2:window-size v)
      (setup-viewport w h)
      (setup-matrices v w h))

    (gl:enable :depth-test)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear-depth 1.0)
    (gl:clear :color-buffer :depth-buffer)

    (setf programs
          (kit.gl.shader:compile-shader-dictionary 'shaders-3.3))

    (let ((hash (make-hash-table)))
      (setf vaos hash)
      (each-node (lambda (m)
                   (let ((verts (mesh-vertices m)))
                     (unless (gethash verts hash)
                       (setf (gethash verts hash)
                             (load-mesh v m)))))
                 stage
                 :filter 'mesh
                 :transform-p nil))))

(defmethod sdl2.kit:keyboard-event
    ((window viewer) state ts repeat-p keysym)
  (declare (ignorable ts state repeat-p))
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (kit.sdl2:close-window window))))

(defmethod sdl2.kit:close-window ((v viewer))
  (with-slots (vaos programs) v
    (apply #'gl-delete (alexandria:hash-table-values vaos))
    (gl-delete programs))
  (call-next-method))

(defun load-mesh (viewer mesh)
  (with-slots (stage programs) viewer
    (let* ((vao (make-instance 'vao :type 'simple-flat :primitive :triangles
                  :vertex-count (* 3 (mesh-flat-vertex-count mesh))))
           (stage-data (static-vectors:make-static-vector
                        (* 3 (mesh-flat-vertex-count mesh))
                        :element-type 'single-float)))
      (expand-indexed (mesh-faces mesh) stage-data (mesh-vertices mesh))
      (vao-buffer-data vao 0
                       (* 4 (length stage-data))
                       (static-vectors:static-vector-pointer stage-data))
      (expand-indexed (mesh-face-normals mesh) stage-data (mesh-normals mesh))
      (vao-buffer-data vao 1
                       (* 4 (length stage-data))
                       (static-vectors:static-vector-pointer stage-data))
      (static-vectors:free-static-vector stage-data)
      vao)))

(defun c*a (color)
  "Multiply `COLOR` by alpha if `COLOR` has an alpha component"
  (etypecase color
    (vec3 color)
    (vec4 (vec* (vec3 color) (aref color 3)))))

(defun draw-mesh (viewer mesh)
  (with-slots (programs vaos) viewer
    (let* ((vao (gethash (mesh-vertices mesh) vaos))
           (mat (if (> (length (mesh-materials mesh)) 0)
                    (material-values (aref (mesh-materials mesh) 0))
                    *default-material*)))
      (kit.gl.shader:uniformfv programs "mat.diffuse_color"
                               (c*a (gethash :diffuse mat)))
      (kit.gl.shader:uniformfv programs "mat.specular_color"
                               (c*a (gethash :specular mat)))
      (kit.gl.shader:uniformf programs "mat.roughness"
                              (gethash :roughness mat))
      (kit.gl.shader:uniformf programs "mat.ior"
                              (gethash :specular-ior mat))
      (vao-draw vao :count (mesh-flat-vertex-count mesh)))))

(defun setup-light (lights n view-matrix dict)
  (flet ((light (attr) (format nil "lights[~D].~A" n attr)))
    (let* ((light (car (aref lights n)))
           (position (kit.glm:transform-point (kit.glm:vec 0.0 0.0 0.0)
                                              (kit.glm:matrix*
                                               view-matrix
                                               (cdr (aref lights n)))))
           (diffuse (light-diffuse light))
           (specular (light-specular light))
           (const (light-attenuation-constant light))
           (lin (light-attenuation-linear light))
           (quad (light-attenuation-quadratic light)))
      (kit.gl.shader:uniformfv dict (light "position") position)
      (kit.gl.shader:uniformfv dict (light "diffuse") diffuse)
      (kit.gl.shader:uniformfv dict (light "specular") specular)
      (kit.gl.shader:uniformf  dict (light "atten_const") const)
      (kit.gl.shader:uniformf  dict (light "atten_lin") lin)
      (kit.gl.shader:uniformf  dict (light "atten_quad") quad))))

(defmethod sdl2.kit:render ((v viewer))
  (with-slots (stage view-matrix projection-matrix vao lights programs) v
    (kit.gl.shader:use-program programs :color)

    (setup-light lights 0 view-matrix programs)
    (gl:clear :color-buffer :depth-buffer)
    (each-node (lambda (m tfm)
                 (kit.gl.shader:uniform-matrix programs :mvp-m 4
                                               (vector (kit.glm:matrix* projection-matrix tfm)))
                 (kit.gl.shader:uniform-matrix programs :mv-m 4
                                               (vector tfm))
                 (kit.gl.shader:uniform-matrix programs :normal-m 4
                                               (vector (kit.glm:transpose-matrix
                                                        (kit.glm:inverse-matrix tfm))))
                 (draw-mesh v m))
               stage
               :filter 'mesh
               :transform view-matrix)))
