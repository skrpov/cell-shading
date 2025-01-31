#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <cglm/cglm.h>

#define CGLTF_IMPLEMENTATION
#include "cgltf.h"

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define FILE_NAME __FILE_NAME__
#define LOG_INFO(msg, ...) fprintf(stderr, "%s:%d:info: " msg "\n", FILE_NAME, __LINE__ ,##__VA_ARGS__)
#define LOG_WARNING(msg, ...) fprintf(stderr, "%s:%d:warning: " msg "\n", FILE_NAME, __LINE__ ,##__VA_ARGS__)
#define LOG_ERROR(msg, ...) fprintf(stderr, "%s:%d:error: " msg "\n", FILE_NAME, __LINE__ ,##__VA_ARGS__)

#define GL_CHECK(call) \
do { \
    call; \
    int err = glGetError(); \
    if (err != GL_NO_ERROR) { \
        LOG_ERROR("%s - %d", #call, err); \
        __builtin_debugtrap(); \
    } \
} while (false)

static uint32_t CompileProgram(const char *vertsource, const char *fragsource) {
    char infolog[1024];
    int length;

    uint32_t vertshader = glCreateShader(GL_VERTEX_SHADER);
    GL_CHECK(glShaderSource(vertshader, 1, &vertsource, NULL));
    GL_CHECK(glCompileShader(vertshader));
    GL_CHECK(glGetShaderInfoLog(vertshader, sizeof(infolog), &length, infolog));
    if (length) {
        LOG_ERROR("%s", infolog);
    }
    uint32_t fragshader = glCreateShader(GL_FRAGMENT_SHADER);
    GL_CHECK(glShaderSource(fragshader, 1, &fragsource, NULL));
    GL_CHECK(glCompileShader(fragshader));
    GL_CHECK(glGetShaderInfoLog(fragshader, sizeof(infolog), &length, infolog));
    if (length) {
        LOG_ERROR("%s", infolog);
    }
    uint32_t program = glCreateProgram();
    GL_CHECK(glAttachShader(program, vertshader));
    GL_CHECK(glAttachShader(program, fragshader));
    GL_CHECK(glLinkProgram(program));
    GL_CHECK(glGetProgramInfoLog(program, sizeof(infolog), &length, infolog));
    if (length) {
        LOG_ERROR("%s", infolog);
    }

    glDeleteShader(fragshader);
    glDeleteShader(vertshader);
    return program;
}

static const char *quadVertSource = 
"#version 330 core\n"

"out vec2 vtexcoord;\n"

"const vec2 positions[6] = vec2[6](\n"
"   vec2(-1, -1), vec2(1, -1), vec2(1, 1),\n"
"   vec2(1, 1), vec2(-1, 1), vec2(-1, -1)\n"
");\n"

"const vec2 texcoords[6] = vec2[6](\n"
"   vec2(0, 0), vec2(1, 0), vec2(1, 1),\n"
"   vec2(1, 1), vec2(0, 1), vec2(0, 0)\n"
");\n"

"void main() {\n"
"   vtexcoord = texcoords[gl_VertexID];\n"
"   gl_Position = vec4(positions[gl_VertexID], 0.0f, 1.0f);\n"
"}\n"
;

static const char *drawTextureFragSource = 
"#version 330 core\n"
"in vec2 vtexcoord;\n"
"out vec4 FragColor;\n"
"uniform sampler2D tex;\n"
"void main() {\n"
"   FragColor = vec4(texture(tex, vtexcoord).rgb, 1.0f);\n"
"}\n"
;

static const char *sobelFragSource= 
"#version 330 core\n"
"in vec2 vtexcoord;\n"
"out vec4 FragColor;\n"
"uniform sampler2D diffuse;\n"
"uniform sampler2D depth;\n"
"const mat3 sy = mat3(\n"
"    1, 0, -1,\n"
"    2, 0, -2,\n"
"    1, 0, -1\n"
");\n"
"const mat3 sx = mat3(\n"
"    1, 2, 1,\n"
"    0, 0, 0,\n"
"    -1, -2, -1\n"
");\n"
"void main() {\n"
"    vec3 diffuseColor = vec3(0);\n"
"    mat3 I;\n"
"    for (int y = 0; y < 3; ++y) {\n"
"        for (int x = 0; x < 3; ++x) {\n"
// "           diffuseColor = max(diffuseColor, texelFetch(diffuse, ivec2(gl_FragCoord) + ivec2(x-1, y-1), 0).rgb);\n"
"           diffuseColor = diffuseColor + texelFetch(diffuse, ivec2(gl_FragCoord) + ivec2(x-1, y-1), 0).rgb;\n"
"           I[y][x] = texelFetch(depth, ivec2(gl_FragCoord) + ivec2(x-1, y-1), 0).r;\n"
"        }\n"
"    }\n"
"    float gx = dot(sx[0], I[0]) + dot(sx[1], I[1]) + dot(sx[2], I[2]); \n"
"    float gy = dot(sy[0], I[0]) + dot(sy[1], I[1]) + dot(sy[2], I[2]);\n"
"    float g = sqrt(pow(gx, 2.0)+pow(gy, 2.0));\n"
"    FragColor= vec4(diffuseColor/9 - pow(1024*vec3(g), vec3(4.0f)), 1.0);\n"
"}\n"
;


static const char *vertsource = 
"#version 330 core\n"
"layout (location = 0) in vec3 aposition;\n"
"layout (location = 1) in vec3 anormal;\n"
"layout (location = 2) in vec2 atexcoord;\n"

"uniform mat4 modelToWorld;\n"
"uniform mat4 worldToClip;\n"

"out vec3 vposition;\n"
"out vec3 vnormal;\n"
"out vec2 vtexcoord;\n"

"void main() {\n"
"   vposition = vec3(modelToWorld * vec4(aposition, 1.0f));\n"
"   vnormal = transpose(inverse(mat3(modelToWorld))) * anormal;\n"
"   vtexcoord = atexcoord;\n"
"   gl_Position = worldToClip * modelToWorld * vec4(aposition, 1.0f);\n"
"}\n"
;

static const char *fragsource = 
"#version 330 core\n"
"in vec3 vposition;\n"
"in vec3 vnormal;\n"
"in vec2 vtexcoord;\n"

"out vec4 FragColor;\n"

"uniform sampler2D baseColorTex;\n"
"uniform vec4 baseColorFactor;\n"

"const int toonLevelCount = 3;\n"
"const float toonScaleFactor = 1.0f/toonLevelCount;\n"

"#define PI 3.141592\n"

"void main() {\n"
"   vec3 sunColor = vec3(1.0f);\n"
"   vec3 sunDir = vec3(1.0f, 1.0f, 1.0f);\n"
"   vec3 cameraPos = vec3(0, 0, 4);\n"

"   vec3 baseColor = baseColorFactor.rgb * pow(texture(baseColorTex, vtexcoord).rgb, vec3(2.2));\n"
"   vec3 L = normalize(sunDir);\n"
"   vec3 N = normalize(vnormal);\n"
"   vec3 V = normalize(cameraPos - vposition);\n"

"   float lambert = max(dot(L, N), 0.0f);\n"
"   float diffuseFactor = + 0.1f + 0.9f*ceil(lambert * toonLevelCount) * toonScaleFactor;\n"
"   vec3 diffuseColor = sunColor*baseColor*diffuseFactor;\n"

"   FragColor = pow(vec4(diffuseColor, 1.0f), vec4(1/2.2));\n"
// "   FragColor = pow(vec4((baseColor/PI)*(sunColor*lambert), 1.0f), vec4(1/2.2));\n"
"}\n"
;

typedef struct {
    GLenum indexType;
    uint32_t vertexArray;
    uint32_t positionBuffer;
    uint32_t normalBuffer;
    uint32_t texcoordBuffer;
    uint32_t indexBuffer;
    size_t indexCount;
} Mesh;

static void CleanupMesh(Mesh *mesh) {
    glDeleteVertexArrays(1, &mesh->vertexArray);
    glDeleteBuffers(1, &mesh->positionBuffer);
    glDeleteBuffers(1, &mesh->normalBuffer);
    glDeleteBuffers(1, &mesh->indexBuffer);
    mesh->indexCount = 0;
}

typedef struct { 
    uint32_t baseColorTex;
    vec4 baseColorFactor;

} Material;

typedef struct {
    cgltf_data *data;
    Mesh meshes[256];
    size_t meshCount; 

    Material materials[256];
    size_t materialCount;
} GltfModel;

static void CleanupGltfModel(GltfModel *model) {
    while (model->meshCount--) {
        CleanupMesh(&model->meshes[model->meshCount]);
    }
    cgltf_free(model->data);
}

static uint32_t CreateWhiteTexture(void) {
    uint32_t tex;
    glGenTextures(1, &tex);
    GL_CHECK(glBindTexture(GL_TEXTURE_2D, tex));

    uint32_t white = 0xffffffff;
    GL_CHECK(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, &white));

    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT););
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST));

    return white;
}

static uint32_t CreateTextureFromTextureView(const cgltf_texture_view *textureView) {
    const cgltf_texture *texture = textureView->texture;
    // HACK: Some files will still gete the `has_pbr_metaillic_roughness` but then won't have an 
    // albedo map, because they are using a flat color or whatever. So this should fallback to an all white texture.
    // honestly this should probobly exist on the level of `Material` because I don't want to create a million white textures 
    // for something which should just be a flat color.
    // 
    // enum MaterialFlags {
    //   MATERIAL_FLAGS_ALBEDO_MAP_BIT  = 0x1,
    //   MATERIAL_FLAGS_NORMAL_MAP_BIT  = 0x2,
    //   MATERIAL_FLAGS_TRANSPARENT_BIT = 0x4,
    // };
    //
    // struct Material {
    //   uint32_t typeMask;
    //   ...
    // }
    assert(texture);

    const cgltf_image *image = texture->image;
    const cgltf_sampler *sampler = texture->sampler;

    // HACK: Only handling texture data supplied in a .glb, and not loading externally.
    // To fix this the function can either directly do IO and try to handle all of the associated errors, 
    // or IO can be done in bulk by another system, and then .. I basically get the same case where I need to check if the 
    // file exists then provide a good error message if it doesn't and provide a fallback.
    assert(image->buffer_view);

    const cgltf_buffer_view *bufferView = image->buffer_view;
    const cgltf_buffer *buffer = bufferView->buffer;
    const uint8_t *data = (const uint8_t *)buffer->data + bufferView->offset;

    int width, height;  
    uint8_t *imageData = stbi_load_from_memory(data, bufferView->size, &width, &height, NULL, 4);
    assert(imageData);

    uint32_t tex;
    glGenTextures(1, &tex);
    GL_CHECK(glBindTexture(GL_TEXTURE_2D, tex));
    GL_CHECK(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, imageData));

    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT););
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));

    // TODO: I don't really want mipmaps right now
    // if (sampler->wrap_s) GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, sampler->wrap_s););
    // if (sampler->wrap_t) GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, sampler->wrap_t));
    // if (sampler->min_filter) GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, sampler->min_filter));
    // if (sampler->mag_filter) GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, sampler->mag_filter));

    return tex;
}

static Material CreatePinkCheckerMaterial(void) {
    uint32_t tex;
    glGenTextures(1, &tex);
    GL_CHECK(glBindTexture(GL_TEXTURE_2D, tex));

    const uint32_t pattern[] = {0xffff00ff, 0xff000000, 0xffff00ff, 0xff000000};
    GL_CHECK(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, 0, GL_RGBA, GL_UNSIGNED_BYTE, &pattern[0]));

    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT););
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST));
    GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST));

    return (Material) {
        .baseColorTex = tex,
        .baseColorFactor = {1.0f, 1.0f, 1.0f, 1.0f},
    };
}

static bool LoadGltfModel(GltfModel *model, const char *path) {
    cgltf_options opts = {0};
    cgltf_data *data = NULL;
    cgltf_result result;

    if ((result = cgltf_parse_file(&opts, path, &data)) == cgltf_result_success) {
        if ((result = cgltf_load_buffers(&opts, data, path)) == cgltf_result_success) {
            for (const cgltf_material *mat = data->materials;
                    mat != data->materials + data->materials_count;
                    ++mat) {
                if (mat->has_pbr_metallic_roughness) {
                    model->materials[model->materialCount++] = (Material) {
                        .baseColorTex = CreateTextureFromTextureView(&mat->pbr_metallic_roughness.base_color_texture),
                        .baseColorFactor = {
                            mat->pbr_metallic_roughness.base_color_factor[0],
                            mat->pbr_metallic_roughness.base_color_factor[1],
                            mat->pbr_metallic_roughness.base_color_factor[2],
                            mat->pbr_metallic_roughness.base_color_factor[3],
                        },
                    };
                } else {
                    model->materials[model->materialCount++] = CreatePinkCheckerMaterial();
                }
            }

            for (const cgltf_mesh *mesh = data->meshes; 
                    mesh != data->meshes + data->meshes_count;
                    ++mesh) {

                // Primitives are super weird as always because nodes can references meshes, 
                // so I need some mapping from cgltf_mesh -> cgltf_primitive, and I can't just convert them all to my own thing...
                // I guess I could just traverse in the same order and store a giant list of primitives, then I can traverse meshes on draw and get the 
                // same order while also not having this concept exist in my own code..

                for (const cgltf_primitive *prim = mesh->primitives;
                        prim != mesh->primitives + mesh->primitives_count;
                        ++prim) {
                    Mesh m = {0};
                    glGenVertexArrays(1, &m.vertexArray);
                    glBindVertexArray(m.vertexArray);

                    for (const cgltf_attribute *attrib = prim->attributes;
                            attrib != prim->attributes + prim->attributes_count;
                            ++attrib) {
                        const cgltf_accessor *accessor = attrib->data;
                        const cgltf_buffer_view *bufferView  = accessor->buffer_view;
                        const cgltf_buffer *buffer = bufferView->buffer;
                        const uint8_t *data = (const uint8_t *)buffer->data + bufferView->offset + accessor->offset;

                        // TODO: The way the buffers are just being loaded doesn't work anymore if something needs to be 
                        // generated, like normals for example.

                        switch (attrib->type) {
                        case cgltf_attribute_type_position: {
                            assert(m.positionBuffer == 0);
                            GL_CHECK(glGenBuffers(1, &m.positionBuffer));
                            GL_CHECK(glBindBuffer(GL_ARRAY_BUFFER, m.positionBuffer));
                            GL_CHECK(glBufferData(GL_ARRAY_BUFFER, sizeof(float)*3*accessor->count, data, GL_STATIC_DRAW));

                            GL_CHECK(glEnableVertexAttribArray(0));
                            GL_CHECK(glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (const void *)0));
                        } break;
                        case cgltf_attribute_type_normal: {
                            assert(m.normalBuffer== 0);
                            GL_CHECK(glGenBuffers(1, &m.normalBuffer));
                            GL_CHECK(glBindBuffer(GL_ARRAY_BUFFER, m.normalBuffer));
                            GL_CHECK(glBufferData(GL_ARRAY_BUFFER, sizeof(float)*3*accessor->count, data, GL_STATIC_DRAW));

                            GL_CHECK(glEnableVertexAttribArray(1));
                            GL_CHECK(glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (const void *)0));
                        } break;
                        case cgltf_attribute_type_texcoord: {
                            assert(m.texcoordBuffer== 0);
                            GL_CHECK(glGenBuffers(1, &m.texcoordBuffer));
                            GL_CHECK(glBindBuffer(GL_ARRAY_BUFFER, m.texcoordBuffer));
                            GL_CHECK(glBufferData(GL_ARRAY_BUFFER, sizeof(float)*2*accessor->count, data, GL_STATIC_DRAW));

                            GL_CHECK(glEnableVertexAttribArray(2));
                            GL_CHECK(glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 0, (const void *)0));
                        } break;
                        default: {
                            LOG_WARNING("Ignoring attribute %s", attrib->name);
                        } break;
                        }
                    }

                    if (prim->indices) {
                        const cgltf_accessor *accessor = prim->indices;
                        const cgltf_buffer_view *bufferView  = accessor->buffer_view;
                        const cgltf_buffer *buffer = bufferView->buffer;
                        uint8_t *data = (uint8_t *)buffer->data + bufferView->offset + accessor->offset;

                        GL_CHECK(glGenBuffers(1, &m.indexBuffer));
                        GL_CHECK(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m.indexBuffer));
                        GL_CHECK(glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(uint32_t)*accessor->count, data, GL_STATIC_DRAW));
                        m.indexCount = accessor->count;

                        if (accessor->component_type == cgltf_component_type_r_32u) {
                            m.indexType = GL_UNSIGNED_INT;
                        } else if (accessor->component_type == cgltf_component_type_r_16u) {
                            m.indexType = GL_UNSIGNED_SHORT;
                        } else {
                            LOG_WARNING("mesh %zu unhandled index type %u", model->meshCount, accessor->component_type);
                        }
                    } else {
                        LOG_WARNING("mesh %zu is missing indices", model->meshCount);
                    }

                    assert(model->meshCount < (sizeof(model->meshes)/sizeof(model->meshes[0])));
                    model->meshes[model->meshCount++] = m;
                }
            }

            model->data = data;
            return true;
            
        } else {
            LOG_ERROR("Couldn't load buffers for gltf file \"%s\"", path);
        }

        cgltf_free(data);
    } else {
        LOG_ERROR("Couldn't load gltf file \"%s\"", path);
    }

    return false;
}

static void RenderGltfNode(GltfModel *model, mat4 parentMatrix, uint32_t shader, size_t *firstPrim, const cgltf_node *node) {
    mat4 localMatrix = GLM_MAT4_IDENTITY_INIT;

    mat4 nodeMatrix;
    glm_mat4_mul(parentMatrix, localMatrix, nodeMatrix);

    if (node->mesh) {
        for (size_t i = 0; i < node->mesh->primitives_count; ++i) {
            size_t materialIndex = cgltf_material_index(model->data, node->mesh->primitives[i].material);
            Material *mat = &model->materials[materialIndex];

            Mesh *mesh = &model->meshes[(*firstPrim)++];
            assert(mesh->positionBuffer != 0);
            assert(mesh->normalBuffer != 0);
            assert(mesh->indexBuffer != 0);
            assert(mesh->indexCount > 0);

            GL_CHECK(glUniformMatrix4fv(glGetUniformLocation(shader, "modelToWorld"), 1, GL_FALSE, &nodeMatrix[0][0]));
            GL_CHECK(glUniform1i(glGetUniformLocation(shader, "baseColorTex"), 0));
            GL_CHECK(glUniform4fv(glGetUniformLocation(shader, "baseColorFactor"), 1, &mat->baseColorFactor[0]));

            GL_CHECK(glActiveTexture(GL_TEXTURE0 + 0));
            GL_CHECK(glBindTexture(GL_TEXTURE_2D, mat->baseColorTex));

            GL_CHECK(glBindVertexArray(mesh->vertexArray));
            GL_CHECK(glDrawElements(GL_TRIANGLES, mesh->indexCount, mesh->indexType, 0));
        }
    }

    for (size_t i = 0; i < node->children_count; ++i) {
        RenderGltfNode(model, nodeMatrix, shader, firstPrim, node->children[i]);
    }
}

static void RenderGltfModel(GltfModel *model, mat4 parentMatrix, uint32_t shader) {
    const cgltf_data *data = model->data;
    size_t firstPrim = 0;
    for (const cgltf_node *node = data->nodes;
            node != data->nodes + data->nodes_count;
            ++node) {
        if (node->parent == NULL) {
            RenderGltfNode(model, parentMatrix, shader, &firstPrim, node);
        }
    }
}

typedef struct {
    vec3 position;
    vec3 up;
    float yaw;
    float pitch;
} Camera;

static Camera MakeDefaultCamera(void) {
    return (Camera) {
        .position = {0, 0, 4},
        .up = {0, 1, 0},
        .yaw = (float)-M_PI_2,
        .pitch = 0.0f,
    };
}

static void GetCameraDirection(Camera camera, vec3 outDir) {
    outDir[0] = cosf(camera.yaw) * cosf(camera.pitch);
    outDir[1] = sinf(camera.pitch);
    outDir[2] = sinf(camera.yaw) * cosf(camera.pitch);
}

void GlfwErrorCallback(int code, const char *msg) {
    (void)code;
    LOG_ERROR("%s", msg);
}

bool Run(const char *path) {
    bool success = true;

    glfwSetErrorCallback(GlfwErrorCallback);
    if (glfwInit()) {
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

        int width = 800;
        int height = 600;
        GLFWwindow *window = glfwCreateWindow(width, height, "", NULL, NULL);
        if (window) {
            glfwMakeContextCurrent(window);
            if (gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) {

                GltfModel model = {0};
                if (LoadGltfModel(&model, path)) {
                    uint32_t fullscreenQuadTextureShader = CompileProgram(quadVertSource, sobelFragSource);
                    uint32_t defaultShader = CompileProgram(vertsource, fragsource);
                    Camera camera = MakeDefaultCamera();

                    // Refresh the width, height to reflect the framebuffer rather than the window.
                    glfwGetFramebufferSize(window, &width, &height);

                    uint32_t fbo = 0;
                    uint32_t renderTex = 0;
                    uint32_t depthAttachment = 0;

                    

                    bool shouldResize = true;
                    double lastTime = glfwGetTime();
                    while (!glfwWindowShouldClose(window)) {
                        glfwPollEvents();

                        double now = glfwGetTime();
                        double dt = now - lastTime;
                        lastTime = now;

                        int newWidth, newHeight;
                        glfwGetFramebufferSize(window, &newWidth, &newHeight);
                        if (width != newWidth || height != newHeight) {
                            shouldResize = true;
                        }

                        if (shouldResize) {
                            width = newWidth;
                            height = newHeight;

                            if (depthAttachment) {
                                GL_CHECK(glDeleteTextures(1, &depthAttachment));
                            }

                            if (renderTex) {
                                GL_CHECK(glDeleteTextures(1, &renderTex));
                            }

                            if (fbo) {
                                GL_CHECK(glDeleteFramebuffers(1, &fbo));
                            }

                            glGenFramebuffers(1, &fbo);
                            glBindFramebuffer(GL_FRAMEBUFFER, fbo);

                            GL_CHECK(glGenTextures(1, &renderTex));
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, renderTex));
                            GL_CHECK(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL));
                            GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR););
                            GL_CHECK(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, 0));

                            GL_CHECK(glGenTextures(1, &depthAttachment));
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, depthAttachment));
                            GL_CHECK(glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL));
                            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
                            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
                            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT); 
                            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, 0));

                                                GL_CHECK(glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, renderTex, 0); );
                            GL_CHECK(glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, depthAttachment, 0););

                            assert(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE);
                            glBindFramebuffer(GL_FRAMEBUFFER, 0);
                        }

                        float aspect = (float)width/height;

                        vec3 cameraDir;
                        GetCameraDirection(camera, cameraDir);

                        mat4 P, V;
                        glm_perspective(M_PI_4, aspect, 0.1f, 1000.0f, P);
                        glm_look(camera.position, cameraDir, camera.up, V);

                        mat4 worldToClip;
                        glm_mat4_mul(P, V, worldToClip);

                        mat4 M = GLM_MAT4_IDENTITY_INIT;
                        static float rotation = 0.0f;
                        rotation += dt*M_PI_2/3.0f;
                        glm_rotate(M, rotation, camera.up);

                        {
                            GL_CHECK(glBindFramebuffer(GL_FRAMEBUFFER, fbo));
                            GL_CHECK(glEnable(GL_DEPTH_TEST));
                            GL_CHECK(glClearColor(0.5f, 0.5f, 1.0f, 1.0f));
                            GL_CHECK(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));

                            GL_CHECK(glUseProgram(defaultShader));
                            GL_CHECK(glUniformMatrix4fv(glGetUniformLocation(defaultShader, "worldToClip"), 1, GL_FALSE, &worldToClip[0][0]));

                            RenderGltfModel(&model, M, defaultShader);
                        }

                        {
                            GL_CHECK(glBindFramebuffer(GL_FRAMEBUFFER, 0));
                            GL_CHECK(glDisable(GL_DEPTH_TEST));
                            GL_CHECK(glViewport(0, 0, width, height));

                            GL_CHECK(glUseProgram(fullscreenQuadTextureShader));
                            GL_CHECK(glUniform1i(glGetUniformLocation(fullscreenQuadTextureShader, "diffuse"), 0));
                            GL_CHECK(glUniform1i(glGetUniformLocation(fullscreenQuadTextureShader, "depth"), 1));

                            GL_CHECK(glActiveTexture(GL_TEXTURE0 + 0));
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, renderTex));

                            GL_CHECK(glActiveTexture(GL_TEXTURE0 + 1));
                            GL_CHECK(glBindTexture(GL_TEXTURE_2D, depthAttachment));

                            GL_CHECK(glDrawArrays(GL_TRIANGLES, 0, 6));

                            // GL_CHECK(glEnable(GL_FRAMEBUFFER_SRGB));
                            // GL_CHECK(glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo));
                            // GL_CHECK(glReadBuffer(GL_COLOR_ATTACHMENT0));
                            // GL_CHECK(glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0));
                            // GL_CHECK(glBlitFramebuffer(0, 0, width, height, 0, 0, width, height, GL_COLOR_BUFFER_BIT, GL_NEAREST));
                        }

                        glfwSwapBuffers(window);
                    }

                    // glDeleteRenderbuffers(1, &rbo);
                    glDeleteTextures(1, &renderTex);
                    glDeleteFramebuffers(1, &fbo);

                    CleanupGltfModel(&model);
                } else {
                    success = false;
                }

            } else {
                success = false;
            }

            glfwDestroyWindow(window);
        } else {
            success = false;
        }

        glfwTerminate();
    } else {
        success = false;
    }

    return success;
}

int main(int argc, const char *argv[]) {
    if (argc < 2) {
        printf("Usage: cell-shading <path-to-gltf>\n");
        return EXIT_SUCCESS;
    }
    const char *path = argv[1];
    return Run(path) ? EXIT_SUCCESS : EXIT_FAILURE;
}
