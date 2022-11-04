#include "Dict.hpp"
#include <cstdlib>
using namespace communication::datatypes;

Dict::~Dict() {
    clear();
}

void Dict::clear() {
    int i;
    if (vertices != NULL) {
        for (i = 0; i < nvert; i++) {
            if (vertices[i] != NULL) {
                free(vertices[i]);
                vertices[i] = NULL;
            }
        }
        free(vertices);
        vertices = NULL;
    }
    if (vertex_colors != NULL) {
        for (i = 0; i < nvert; i++) {
            if (vertex_colors[i] != NULL) {
                free(vertex_colors[i]);
                vertex_colors[i] = NULL;
            }
        }
        free(vertex_colors);
        vertex_colors = NULL;
    }

    if (faces != NULL) {
        for (i = 0; i < nface; i++) {
            if (faces[i] != NULL) {
                free(faces[i]);
                faces[i] = NULL;
            }
        }
        free(faces);
        faces = NULL;
    }

    delete nvert_in_face;
    vertices = nullptr;
    faces = nullptr;
    vertex_colors = nullptr;
    nvert_in_face = nullptr;
}