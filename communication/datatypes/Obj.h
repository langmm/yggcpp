#pragma once
#include <cstdlib>
#include "dtype_t.hpp"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct dtype_t obj_t;

obj_t init_obj();

void free_obj(obj_t *p);

int alloc_obj(obj_t *p, int nvert, int ntexc, int nnorm, int nparam,
              int npoint, int nline, int nface, int ncurve, int ncurve2,
              int nsurf, int do_color);

obj_t copy_obj(obj_t src);

void display_obj_indent(obj_t p, const char* indent);

void display_obj(obj_t p);

size_t obj_nvert(const obj_t* obj);
size_t obj_ntexc(const obj_t* obj);
size_t obj_nnorm(const obj_t* obj);
size_t obj_nparam(const obj_t* obj);
size_t obj_npoint(const obj_t* obj);
size_t obj_nline(const obj_t* obj);
size_t obj_nface(const obj_t* obj);
size_t obj_ncurve(const obj_t* obj);
size_t obj_ncurve2(const obj_t* obj);
size_t obj_nsurf(const obj_t* obj);

void obj_vertices(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_texcoords(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_normals(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_params(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_curve_params(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_surface_params_u(const obj_t* obj, float** data, size_t &N, size_t &M);
void obj_surface_params_v(const obj_t* obj, float** data, size_t &N, size_t &M);

void obj_vertex_colors(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_points(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nvert_in_point(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_lines(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nvert_in_line(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_line_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_faces(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nvert_in_face(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_face_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_face_normals(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_curves(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nvert_in_curve(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_curves2(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nparam_in_curve2(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_surfaces(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_surface_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_surface_normals(const obj_t* obj, int** data, size_t &N, size_t &M);
void obj_nvert_in_surface(const obj_t* obj, int** data, size_t &N, size_t &M);

#ifdef __cplusplus
}
#endif

