#include "PlyDict.hpp"
#include "utils/tools.hpp"
using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

PlyDict::PlyDict() : Dict() {
    nedge = 0;
    edges = nullptr;
    edge_colors = nullptr;
}

PlyDict::~PlyDict() {
    int i;
    if (edges != nullptr) {
        for (i = 0; i < nedge; i++) {
            if (edges[i] != nullptr) {
                free(edges[i]);
                edges[i] = nullptr;
            }
        }
        free(edges);
        edges = nullptr;
    }
    if (edge_colors != nullptr) {
        for (i = 0; i < nedge; i++) {
            if (edge_colors[i] != nullptr) {
                free(edge_colors[i]);
                edge_colors[i] = nullptr;
            }
        }
        free(edge_colors);
        edge_colors = nullptr;
    }
}

int PlyDict::alloc(int nvert, int nface, int nedge, int do_vert_color, int do_edge_color) {
    int i;
    free_ply(p); // Ensure that existing data is freed
    material[0] = '\0';
    this->nvert = nvert;
    this->nface = nface;
    this->nedge = nedge;
    // Allocate vertices
    float **new_vert = (float**)malloc(nvert*sizeof(float*));
    if (new_vert == nullptr) {
        ygglog_error("alloc_ply: Failed to allocate vertices.");
        free_ply(p);
        return -1;
    }
    vertices = new_vert;
    for (i = 0; i < nvert; i++) {
        float *ivert = (float*)malloc(3*sizeof(float));
        if (ivert == nullptr) {
            ygglog_error("alloc_ply: Failed to allocate vertex %d.", i);
            free_ply(p);
            return -1;
        }
        vertices[i] = ivert;
    }
    ygglog_debug("alloc_ply: Allocated %d vertices.", nvert);
    // Allocate vertex colors
    if (do_vert_color) {
        int **new_vert_colors = (int**)malloc(nvert*sizeof(int*));
        if (new_vert_colors == nullptr) {
            ygglog_error("alloc_ply: Failed to allocate vertex_colors.");
            free_ply(p);
            return -1;
        }
        vertex_colors = new_vert_colors;
        for (i = 0; i < nvert; i++) {
            int *ivert = (int*)malloc(3*sizeof(int));
            if (ivert == nullptr) {
                ygglog_error("alloc_ply: Failed to allocate vertex color %d.", i);
                free_ply(p);
                return -1;
            }
            vertex_colors[i] = ivert;
        }
        ygglog_debug("alloc_ply: Allocated %d vertex colors.", nvert);
    }
    // Allocate faces
    int **new_face = (int**)malloc(nface*sizeof(int*));
    if (new_face == nullptr) {
        ygglog_error("alloc_ply: Failed to allocate faces.");
        free_ply(p);
        return -1;
    }
    faces = new_face;
    for (i = 0; i < nface; i++) {
        faces[i] = nullptr;
        /* int *iface = (int*)malloc(3*sizeof(int)); */
        /* if (iface == nullptr) { */
        /*   ygglog_error("alloc_ply: Failed to allocate face %d.", i); */
        /*   free_ply(p); */
        /*   return -1; */
        /* } */
    }
    ygglog_debug("alloc_ply: Allocated %d faces.", nface);
    // Allocate nvert_in_face
    int *new_nvert = (int*)malloc(nface*sizeof(int));
    if (new_nvert == nullptr) {
        ygglog_error("alloc_ply: Failed to allocate nvert_in_face.");
        free_ply(p);
        return -1;
    }
    nvert_in_face = new_nvert;
    for (i = 0; i < nface; i++) {
        nvert_in_face[i] = 0;
    }
    // Allocate edges
    int **new_edge = (int**)malloc(nedge*sizeof(int*));
    if (new_edge == nullptr) {
        ygglog_error("alloc_ply: Failed to allocate edges.");
        free_ply(p);
        return -1;
    }
    edges = new_edge;
    for (i = 0; i < nedge; i++) {
        int *iedge = (int*)malloc(2*sizeof(int));
        if (iedge == nullptr) {
            ygglog_error("alloc_ply: Failed to allocate edge %d.", i);
            free_ply(p);
            return -1;
        }
        edges[i] = iedge;
    }
    ygglog_debug("alloc_ply: Allocated %d edges.", nedge);
    // Allocate edge colors
    if (do_edge_color) {
        int **new_edge_colors = (int**)malloc(nedge*sizeof(int*));
        if (new_edge_colors == nullptr) {
            ygglog_error("alloc_ply: Failed to allocate edge_colors.");
            free_ply(p);
            return -1;
        }
        edge_colors = new_edge_colors;
        for (i = 0; i < nedge; i++) {
            int *iedge = (int*)malloc(3*sizeof(int));
            if (iedge == nullptr) {
                ygglog_error("alloc_ply: Failed to allocate edge color %d.", i);
                free_ply(p);
                return -1;
            }
            edge_colors[i] = iedge;
        }
        ygglog_debug("alloc_ply: Allocated %d edge colors.", nedge);
    }
    ygglog_debug("alloc_ply: Allocate for %d vertices, %d faces, and %d edges.",
                 nvert, nface, nedge);
    return 0;
}

PlyDict::PlyDict(const PlyDict *src) : PlyDict() {
    int i;
    int do_vert_color = 0, do_edge_color = 0;
    if (src->vertex_colors != NULL) {
        do_vert_color = 1;
    }
    if (src->edge_colors != NULL) {
        do_edge_color = 1;
    }

    alloc(src->nvert, src->nface, src->nedge, do_vert_color, do_edge_color);
    strcpy(material, src->material);
    // Copy vertices
    for (i = 0; i < src->nvert; i++) {
        memcpy(vertices[i], src->vertices[i], 3*sizeof(float));
    }
    if (do_vert_color) {
        for (i = 0; i < src->nvert; i++) {
            memcpy(vertex_colors[i], src->vertex_colors[i], 3*sizeof(int));
        }
    }
    // Copy faces
    memcpy(nvert_in_face, src->nvert_in_face, src->nface*sizeof(int));
    for (i = 0; i < src->nface; i++) {
        int *iface = (int*)realloc(faces[i], src->nvert_in_face[i]*sizeof(int));
        if (iface == NULL) {
            ygglog_error("PlyDict::copy_ply: Could not allocate face %d.", i);
            free_ply(&out);
            return;
        }
        faces[i] = iface;
        memcpy(faces[i], src->faces[i], src->nvert_in_face[i]*sizeof(int));
    }
    // Copy edges
    for (i = 0; i < src->nedge; i++) {
        memcpy(edges[i], src->edges[i], 2*sizeof(int));
    }
    if (do_edge_color) {
        for (i = 0; i < src->nedge; i++) {
            memcpy(edge_colors[i], src->edge_colors[i], 3*sizeof(int));
        }
    }
}

void PlyDict::display_indent(const char *indent) const {
    int i, j;
    printf("%s%d Vertices:\n", indent, nvert);
    for (i = 0; i < nvert; i++) {
        printf("%s  %f, %f, %f\n", indent,
               vertices[i][0], vertices[i][1], vertices[i][2]);
    }
    printf("%s%d Edges:\n", indent, nedge);
    for (i = 0; i < nedge; i++) {
        printf("%s  %d, %d\n", indent,
               edges[i][0], edges[i][1]);
    }
    printf("%s%d Faces:\n", indent, nface);
    for (i = 0; i < nface; i++) {
        printf("%s  %d", indent, faces[i][0]);
        for (j = 1; j < nvert_in_face[i]; j++)
            printf(", %d", faces[i][j]);
        printf("\n");
    }
}

void PlyDict::display() const {
    display_indent("");
}