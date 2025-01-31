cmake_minimum_required(VERSION 3.15)

set(CMAKE_C_STANDARD 99)
project(CellShading)

find_package(OpenGL REQUIRED)

add_subdirectory(./vendor/glfw)
add_subdirectory(./vendor/cglm)

add_executable(
    cell-shading 
    ./vendor/glad/src/glad.c
    ./src/main.c)

target_include_directories(
    cell-shading
    PRIVATE
    ./vendor/cglm/include
    ./vendor/glfw/include
    ./vendor/stb/ 
    ./vendor/cgltf/
    ./vendor/glad/include)

target_link_libraries(
    cell-shading
    PRIVATE 
    cglm
    glfw 
    OpenGL::GL)
