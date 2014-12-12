#version 330 core
struct Light {
    vec3 position;
    vec3 diffuse;
    float atten_const;
    float atten_lin;
    float atten_quad;
};

uniform mat4 mvp_m;
uniform mat4 mv_m;
uniform mat4 normal_m;
uniform vec3 color;
uniform unsigned int num_lights;
uniform Light lights[4];

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 normal;

smooth out vec3 v_light;
smooth out vec3 v_normal;
smooth out vec4 v_frag;

void main() {
    gl_Position = mvp_m * vec4(vertex, 1.0);
    v_normal = vec3(normal_m * vec4(normal, 0.0));
    v_light = lights[0].position - vec3(mv_m * vec4(vertex, 1.0));
    v_frag = mv_m * vec4(vertex, 1.0);
}
