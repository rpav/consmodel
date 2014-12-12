#version 330 core
struct Light {
    vec3 position;
    vec3 diffuse;
    vec3 specular;
    float atten_const;
    float atten_lin;
    float atten_quad;
};

struct Material {
    vec3 diffuse_color;
    vec3 specular_color;
    float roughness;
    float ior;
};

// Transforms
uniform mat4 mvp_m;
uniform mat4 mv_m;
uniform mat4 normal_m;

// Material
uniform Material mat;

// Lights
uniform unsigned int num_lights;
uniform Light lights[4];

in vec3 v_normal;
in vec3 v_light;
in vec4 v_frag;
out vec4 f_color;

float PI = 3.141592653589793;

float fresnel_schlick(float cosTheta, float n1, float n2) {
    float F0 = (n1 - n2)/(n1 + n2);
    F0 = pow(F0, 2);

    return F0 + ((1 - F0) * pow(1 - cosTheta, 5));
}

void main() {
    vec3 L = normalize(v_light);
    vec3 N = normalize(v_normal);

    float diffuse = clamp(dot(L, N), 0.0, 1.0);
    float dist = length(vec3(v_frag) - v_light);
    float atten =
        lights[0].atten_const +
        lights[0].atten_lin*dist +
        lights[0].atten_quad*dist*dist;
    vec3 diffuse_color = (lights[0].diffuse * diffuse * mat.diffuse_color);

    float specular = 0.0;
    float NdotL = dot(v_normal, v_light);

    if(NdotL > 0.0 && mat.roughness > 0.0) {
        vec3 V = normalize(-vec3(v_frag));
        vec3 H = normalize(V + L);
        float NdotH = dot(N, H);
        float NdotV = dot(N, V);
        float VdotH = dot(V, H);

        // Roughness R
        float m_sq = pow(mat.roughness,2);
        float r0 = 1.0 / (PI * m_sq * pow(NdotH, 4));
        float r1 = (pow(NdotH, 2) - 1) / (m_sq * pow(NdotH, 2));
        float R = r0 * exp(r1);

        // Geometric Attenuation G
        float G = min(1.0,
                      min((2 * NdotH * NdotV) / VdotH,
                          (2 * NdotH * NdotL) / VdotH));

        specular =
            (fresnel_schlick(VdotH, 1.0, mat.ior) * G * R) /
            (NdotV * NdotL * PI);
    }

    vec3 specular_color = (specular * mat.specular_color);

    f_color = vec4(diffuse_color + specular_color, 1.0);
}
