#pragma once
#include "dtype_t.hpp"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct dtype_t ply_t;

ply_t init_ply();

void free_ply(ply_t *p);

ply_t copy_ply(ply_t src);

void display_ply_indent(ply_t p, const char* indent);

void display_ply(ply_t p);

ply_t* create_ply_from_verts(const float* verts);
int ply_add_vertices(ply_t* ply, const int M, const int N, float* verts);
ply_t* create_ply_from_verts_and_faces(const int VM, const int VN, float* verts,
                                       const int FM, const int FN, int* faces);
int ply_add_faces(ply_t* ply, const int M, const int N, float* faces);
ply_t* create_ply_from_faces(const int M, const int N, int* faces);
ply_t* create_ply_from_edges(const int M, const int N, int* edges);
int ply_add_edges(ply_t* ply, const int M, const int N, int* edges);
ply_t* create_ply_t_from_all(const int VM, const int VN, float* verts,
                             const int FM, const int FN, int* faces,
                             const int EM, const int EN, int* edges);
ply_t* create_ply_t();
void ply_get_double_array(const ply_t* ply, const char* name0, double* data, size_t &len, bool skipcolors=false);
void ply_get_int_array(const ply_t* ply, const char* name0, int* data,  size_t &len, bool skipcolors=false);
bool ply_is_valid(const ply_t* ply);
size_t ply_count_elements(const ply_t* ply, const char* name);
size_t ply_nvert(const ply_t* ply);
size_t ply_nedge(const ply_t* ply);
size_t ply_nface(const ply_t* ply);

#ifdef __cplusplus
}
#endif

