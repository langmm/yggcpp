#pragma once
#include <cstdlib>
#include "dtype_t.hpp"
#ifdef __cplusplus
extern "C" {
#endif

/*! @brief Obj structure. */
typedef struct obj_t {
    char material[100]; //!< Material that should be used for faces.
    int nvert; //!< Number of vertices.
    int ntexc; //!< Number of texture coordinates.
    int nnorm; //!< Number of normals.
    int nparam; //!< Number of params.
    int npoint; //!< Number of points.
    int nline; //!< Number of lines.
    int nface; //!< Number of faces.
    int ncurve; //!< Number of curves.
    int ncurve2; //!< Number of curv2.
    int nsurf; //!< Number of surfaces.
    float **vertices; //!< X, Y, Z positions of vertices.
    int **vertex_colors; //!< RGB colors of each vertex.
    float **texcoords; //!< Texture coordinates.
    float **normals; //!< X, Y, Z direction of normals.
    float **params; //!< U, V, W directions of params.
    int **points; //!< Sets of one or more vertex indices.
    int *nvert_in_point; //!< Number of vertex indices in each point set.
    int **lines; //!< Indices of the vertices composing each line.
    int *nvert_in_line; //!< Number of vertex indices in each line.
    int **line_texcoords; //!< Indices of texcoords for each line vertex.
    int **faces; //!< Indices of the vertices composing each face.
    int *nvert_in_face; //!< Number of vertex indices in each face.
    int **face_texcoords; //!< Indices of texcoords for each face vertex.
    int **face_normals; //!< Indices of normals for each face vertex.
    int **curves; //!< Indices of control point vertices for each curve.
    float **curve_params; //!< Starting and ending parameters for each curve.
    int *nvert_in_curve; //!< Number of vertex indices in each curve.
    int **curves2; //!< Indices of control parameters for each curve.
    int *nparam_in_curve2; //!< Number of parameter indices in each curve.
    int **surfaces; //!< Indices of control point vertices for each surface.
    int *nvert_in_surface; //!< Number of vertices in each surface.
    float **surface_params_u; //!< Starting and ending parameters for each curve in the u direction.
    float **surface_params_v; //!< Starting and ending parameters for each curve in the v direction.
    int **surface_texcoords; //!< Indices of texcoords for each surface vertex.
    int **surface_normals; //!< Indices of normals for each surface vertex.
} obj_t;

dtype_t* wrap_obj(obj_t* obj);
obj_t init_obj();

void free_obj(obj_t *p);

int alloc_obj(obj_t *p, int nvert, int ntexc, int nnorm, int nparam,
              int npoint, int nline, int nface, int ncurve, int ncurve2,
              int nsurf, int do_color);

obj_t copy_obj(obj_t src);

void display_obj_indent(obj_t p, const char* indent);

void display_obj(obj_t p);
#ifdef __cplusplus
}
#endif

