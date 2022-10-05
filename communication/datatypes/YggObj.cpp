#include "YggObj.hpp"
#include "Obj.h"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {

size_t YggObj::nvert() {
    size_t val;
    if (get_property("nvert", val))
        return val;
    return 0;
}
size_t YggObj::ntexc() {
    size_t val;
    if (get_property("ntexc", val))
        return val;
    return 0;
}
size_t YggObj::nnorm() {
    size_t val;
    if (get_property("nnorm", val))
        return val;
    return 0;
}
size_t YggObj::nparam() {
    size_t val;
    if (get_property("nparam", val))
        return val;
    return 0;
}
size_t YggObj::npoint() {
    size_t val;
    if (get_property("npoint", val))
        return val;
    return 0;
}
size_t YggObj::nline() {
    size_t val;
    if (get_property("nline", val))
        return val;
    return 0;
}
size_t YggObj::nface() {
    size_t val;
    if (get_property("nface", val))
        return val;
    return 0;
}
size_t YggObj::ncurve() {
    size_t val;
    if (get_property("ncurve", val))
        return val;
    return 0;
}
size_t YggObj::ncurve2() {
    size_t val;
    if (get_property("ncurve2", val))
        return val;
    return 0;
}
size_t YggObj::nsurf() {
    size_t val;
    if (get_property("nsurf", val))
        return val;
    return 0;
}

std::vector<std::vector<float> > YggObj::vertices() {
	return get_array<float>("vertices");
}
std::vector<std::vector<float> > YggObj::texcoords() {
    return get_array<float>("texcoords");
}
std::vector<std::vector<float> > YggObj::normals() {
    return get_array<float>("normals");
}
std::vector<std::vector<float> > YggObj::params() {
    return get_array<float>("params");
}
std::vector<std::vector<float> > YggObj::curve_params() {
    return get_array<float>("curve_params");
}
std::vector<std::vector<float> > YggObj::surface_params_u() {
    return get_array<float>("surface_params_u");
}
std::vector<std::vector<float> > YggObj::surface_params_v() {
    return get_array<float>("surface_params_v");
}


std::vector<std::vector<int> > YggObj::vertex_colors() {
    return get_array<int>("vertex_colors");
}
std::vector<std::vector<int> > YggObj::points() {
    return get_array<int>("points");
}
std::vector<std::vector<int> > YggObj::nvert_in_point() {
    return get_array<int>("nvert_in_point");
}
std::vector<std::vector<int> > YggObj::lines() {
    return get_array<int>("lines");
}
std::vector<std::vector<int> > YggObj::nvert_in_line() {
    return get_array<int>("nvert_in_line");
}
std::vector<std::vector<int> > YggObj::line_texcoords() {
    return get_array<int>("line_texcoords");
}
std::vector<std::vector<int> > YggObj::faces() {
    return get_array<int>("faces");
}
std::vector<std::vector<int> > YggObj::nvert_in_face() {
    return get_array<int>("nvert_in_face");
}
std::vector<std::vector<int> > YggObj::face_texcoords() {
    return get_array<int>("face_texcoords");
}
std::vector<std::vector<int> > YggObj::face_normals() {
    return get_array<int>("face_normals");
}
std::vector<std::vector<int> > YggObj::curves() {
    return get_array<int>("curves");
}
std::vector<std::vector<int> > YggObj::nvert_in_curve() {
    return get_array<int>("nvert_in_curve");
}
std::vector<std::vector<int> > YggObj::curves2() {
    return get_array<int>("curves2");
}
std::vector<std::vector<int> > YggObj::nparam_in_curve2() {
    return get_array<int>("nparam_in_curve2");
}
std::vector<std::vector<int> > YggObj::surfaces() {
    return get_array<int>("surfaces");
}
std::vector<std::vector<int> > YggObj::surface_texcoords() {
    return get_array<int>("surface_texcoords");
}
std::vector<std::vector<int> > YggObj::surface_normals() {
    return get_array<int>("surface_normals");
}
std::vector<std::vector<int> > YggObj::nvert_in_surface() {
    return get_array<int>("nvert_in_surface");
}

}
}

GET_ITEM(obj,YggObj)

GET_ITEMP(obj,YggObj)

GET_ITEMCP(obj, YggObj)

size_t obj_nvert(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nvert();
}
size_t obj_ntexc(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->ntexc();
}
size_t obj_nnorm(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nnorm();
}
size_t obj_nparam(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nparam();
}
size_t obj_npoint(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->npoint();
}
size_t obj_nline(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nline();
}
size_t obj_nface(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nface();
}
size_t obj_ncurve(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->ncurve();
}
size_t obj_ncurve2(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->ncurve2();
}
size_t obj_nsurf(const obj_t* obj) {
    auto o = get_obj(obj);
    if (o == nullptr)
        return 0;
    return o->nsurf();
}
void obj_vertices(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->vertices();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_texcoords(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->texcoords();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_normals(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->normals();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_params(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->params();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_curve_params(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->curve_params();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_surface_params_u(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->surface_params_u();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_surface_params_v(const obj_t* obj, float** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->surface_params_v();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}

void obj_vertex_colors(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->vertex_colors();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_points(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->points();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nvert_in_point(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nvert_in_point();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_lines(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->lines();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nvert_in_line(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nvert_in_line();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_line_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->line_texcoords();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_faces(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->faces();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nvert_in_face(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nvert_in_face();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_face_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->face_texcoords();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_face_normals(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->face_normals();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_curves(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->curves();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nvert_in_curve(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nvert_in_curve();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_curves2(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->curves2();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nparam_in_curve2(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nparam_in_curve2();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_surfaces(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->surfaces();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_surface_texcoords(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->surface_texcoords();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_surface_normals(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->surface_normals();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
void obj_nvert_in_surface(const obj_t* obj, int** data, size_t &N, size_t &M) {
    auto o = get_obj(obj);
    if (o == nullptr) {
        data = NULL;
        N = 0;
        M = 0;
        return;
    }
    auto vdat = o->nvert_in_surface();
    communication::datatypes::get_data_as_pointers(vdat, data, N, M);
}
