# Consmodel

This is a [CONSPACK](https://github.com/conspack/cl-conspack)-based
format and viewer for 3D model data.  Its purpose is to be a general,
easy-to-load, compact, and extensible model format, primarily targeted
at games.

```console
$ ls -1s cubes.*
640K cubes.blend
 68K cubes.dae
 52K cubes.fbx
 24K cubes.cmdl
```

**This is a work-in-progress.**  I've posted it for experimenting.  The format may yet change quite a bit before it's release-ready.

<img src="http://ogmo.mephle.net/consmodel-window.png"><br>
<a href="http://ogmo.mephle.net/consmodel.png">vs Blender</a>

Requirements:

* [`cl-sdl2`](https://github.com/lispgames/cl-sdl2)
* [`sdl2kit`](https://github.com/lispgames/sdl2kit)
* [`mathkit`](https://github.com/lispgames/mathkit)
* [`glkit`](https://github.com/lispgames/glkit)
* [`classimp`](https://github.com/3b/classimp)
* Other systems available via `QuickLisp`.
* An OpenGL driver capable of GL 3.3+.

Make sure you have the **latest** available from the above; you should almost certainly get the above from their respective repositories.  Currently, both Classimp and viewer functionality/dependencies are "required", but it wouldn't do much without them.

## Usage

Converting files is rather simple:

```lisp
(consmodel:convert-file "INPUT" :output-file "OUTPUT.CMDL")
```

This uses classimp to convert supported formats to the Consmodel format.  You may then view it using the viewer:

```lisp
(consmodel.viewer:view "OUTPUT.CMDL" :w 1280 :h 720)
```

Of course, if you wish to *use* the files, you can load the model via the following:

```lisp
(consmodel:read-cmdl-file "FILE.CMDL") ;; => #<MODEL>
```

## Blender-direct export

In addition to using classimp to convert files, you can directly export files from blender using the [`io_scene_consmodel`](https://github.com/rpav/io_scene_consmodel) addon.

Each method has its pros and cons:

**Conversion:**
Pros:
* Supports a wide variety of formats
* Is not dependent on a particular modeler
* Has numerous conversion options which may be of interest

Cons:
* Assimp seems to have some bugs in the way it performs some conversions
* Certain exporters (specifically in Blender, possibly others), are often incomplete or incorrect; for example , the Collada exporter had poor animation support, but the FBX exporter had numerous incorrect values (transforms, camera, etc), and no other formats appeared to support both.

**Direct export:**
Pros:
* The plugin is developed in parallel and exports known-good values
* The plugin can interface with blender more directly, support any desired values, and have more integration in the asset pipeline

Cons:
* It depends on using Blender

Of course, it's always possible to write plugins for other modelers as well.

## MODEL

The "Model model" is a hierarchy of nodes, each with a local transformation and children.  The `MODEL` itself consists of one or more *stagings* (e.g., a scene in Blender), and stagings may share objects, meshes, materials, etc.  The semantics and usage of stagings are left to the user, however.

Nodes consist of the following:

* `NODE`: This is "blank" object; it may have a local transformation and children, but no other data.
* `MESH`: The core data to be rendered, a mesh has faces, vertices, normals, and materials.
* `LIGHT-POINT`, `LIGHT-DIRECTIONAL`: Both subclasses of `LIGHT`, these represent different kinds of lights.
* `CAMERA`: This can have a transformation matrix, or individual components for establishing a view matrix.

Materials currently consist of `MATERIAL-SIMPLE`, which has a hash table of values.  Other nodes and materials may be added

The model for animations is "coming soon".

To use a model object, one should likely select the desired stage, and extract the data as necessary.  The function `EACH-NODE` is provided for utility in doing this:

```lisp
(each-node FUNCTION NODE
           &key (transform-p T) (transform +identity-matrix+) filter)
```

This iterates each node `NODE` and its children, calling `FUNCTION` on each in order of occurrence.  `FUNCTION` should take two arguments, a node, and a transformation matrix, or if `:transform-p` is `NIL`, only the node argument.

`FILTER` may be provided as a *type specifier*, in which case `FUNCTION` is only called on nodes matching `FILTER`.  All nodes are still iterated internally.

The transformation matrix passed to `FUNCTION` is the cumulative
transformation starting with the root node of the staging.  Note this
is *almost certainly not* a useful view matrix, i.e., by default you
will be in untransformed world space.  To apply a transformation such as your view/projection matrix, you may specify it as `:transform`, and it will be the root transformation.

## Viewer

The viewer is provided both for utility and as sample code to get an idea for how to use consmodel objects.  It's relatively straightforward, but a few things of note:

* This starts by extracting lights and cameras
* Meshes are pre-loaded into VAOs in `initialize-instance` and via `load-mesh`; this is the function that puts the face, vertex, and normal data together.
* `sdl2.kit:render` iterates every mesh using `each-node`
* `draw-mesh` just draws each VAO based on the mesh data; notably VAOs are indexed on *mesh-vertices*, **not** the mesh *node*, because nodes may share vertex data!
* Colors are either premultiplied `vec3` or nonpremultiplied `vec4`.

This is far from complete or perfect however.

* The viewer only renders 1 camera and 1 light.  Adding more isn't a ton of work, and will probably happen soon.
* The shaders are very basic and probably not entirely correct, but a working start.

## TODO

This is, as stated, a work in progress.  Currently working on or toward the following:

* Viewer: More complete rendering, including multiple lights, cameras, and other features as they're added.
* Models: More complete material support, mostly focused around adding UV and texture data to the model file.
* Models: Animation data.
* Models: Lights, spot and directional.

More to come.
