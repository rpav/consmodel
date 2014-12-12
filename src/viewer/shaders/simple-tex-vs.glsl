#version 120
uniform sampler2D texid;

attribute vec3 position;
attribute vec2 vertex_uv;

varying vec2 frag_uv;

void main() {
    frag_uv = vertex_uv;
    gl_Position = gl_ModelViewProjectionMatrix * vec4(position, 1);
}
