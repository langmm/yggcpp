#include "YggPly.hpp"
#include "Ply.h"
#include "utils/tools.hpp"

#include <algorithm>
#include <vector>

namespace communication {
namespace datatypes {

YggPly::YggPly() : rapidjson::Ply() {
}

YggPly::YggPly(const YggPly *src) : YggPly() {
    comments = src->comments;
    format = src->format;
    elements = src->elements;
    element_order = src->element_order;
}

YggPly::~YggPly() {
    clear();
}

void YggPly::clear() {
    comments.clear();
    format = "";
    elements.clear();
    element_order.clear();
}

void YggPly::display_indent(const char *indent) const {
    int j;
    printf("%s%d Vertices:\n", indent, static_cast<int>(count_elements("vertex")));
    for (auto v : get_element_set("vertex")->elements) {
        std::vector<double> e = v.get_double_array(false, 3);
        printf("%s  %f, %f, %f\n", indent,
               e[0], e[1], e[2]);
    }

    printf("%s%d Edges:\n", indent, static_cast<int>(count_elements("edge")));
    for (auto v : get_element_set("edge")->elements) {
        std::vector<int> e = v.get_int_array(false, 3);
        printf("%s  %d, %d\n", indent,
               e[0], e[1]);
    }

    printf("%s%d Faces:\n", indent, static_cast<int>(count_elements("face")));
    for (auto v : get_element_set("face")->elements) {
        std::vector<int> e = v.get_int_array();
        printf("%s  %d", indent, e[0]);
        for (j = 1; j < e.size(); j++) {
            printf(", %d", e[j]);
        }
        printf("\n");
    }
}

void YggPly::display() const {
    display_indent("");
}
}
}

GET_ITEM(ply,YggPly)

GET_ITEMP(ply,YggPly)

GET_ITEMCP(ply, YggPly)

ply_t init_ply() {
    ply_t x;
    auto p = new communication::datatypes::YggPly();
    p->add_element_set("edge");
    p->add_element_set("vertex");
    p->add_element_set("face");
    x.obj = static_cast<void*>(p);
    return x;
}

void free_ply(ply_t *p) {
    delete get_ply(p);
    free(p);
}

ply_t copy_ply(ply_t src) {
    ply_t out;
    out.type = T_PLY;

    auto psrc = get_ply(src);
    if (psrc == nullptr)
        return out;
    auto ply = new communication::datatypes::YggPly(psrc);
    out.obj = static_cast<void*>(ply);
    return out;
}

void display_ply_indent(ply_t p, const char* indent) {
    auto ply = get_ply(p);
    if (ply == nullptr)
        return;
    ply->display_indent(indent);
}

void display_ply(ply_t p) {
    auto ply = get_ply(p);
    if (ply == nullptr)
        return;
    ply->display();
}

ply_t* create_ply_from_verts(const int M, const int N, float* verts) {
    auto out = (ply_t*) malloc(sizeof(ply_t));

    if (out == nullptr)
        return nullptr;
    out->type = T_PLY;
    auto ply = new communication::datatypes::YggPly();
    if (verts != nullptr) {
        ply->add_element_set("vertex", verts, M, N);
    } else {
        ply->add_element_set("vertex");
    }
    ply->add_element_set("edge");
    ply->add_element_set("face");
    out->obj = ply;
    return out;
}

int ply_add_vertices(ply_t* ply, float* verts, const size_t &len) {
    auto pl = get_ply(ply);
    if (pl == nullptr)
        return -1;
    pl->add_element("vertex", std::vector<float>(verts, verts + len));
    return 0;
}

ply_t* create_ply_from_verts_and_faces(const int VM, const int VN, float* verts,
                                       const int FM, const int FN, int* faces) {
    auto out = (ply_t*) malloc(sizeof(ply_t));

    if (out == nullptr)
        return nullptr;
    out->type = T_PLY;
    auto ply = new communication::datatypes::YggPly();
    if (verts != nullptr) {
        ply->add_element_set("vertex", verts, VM, VN);
    } else {
        ply->add_element_set("vertex");
    }
    if (faces != nullptr) {
        ply->add_element_set("face", faces, FM, FN);
    } else {
        ply->add_element_set("face");
    }
    ply->add_element_set("edge");
    out->obj = ply;
    return out;
}

int ply_add_faces(ply_t* ply, int* faces, const size_t &len) {
    auto p = get_ply(ply);
    p->add_element("face", std::vector<int>(faces, faces + len));
    return 0;
}

ply_t* create_ply_from_faces(const int M, const int N, int* faces) {
    auto out = (ply_t*) malloc(sizeof(ply_t));
    out->type = T_PLY;
    auto ply = new communication::datatypes::YggPly();
    ply->add_element_set("edge");
    ply->add_element_set("vertex");

    if (faces != nullptr) {
        ply->add_element_set("face", faces, M, N);
    } else {
        ply->add_element_set("face");
    }

    out->obj = ply;
    return out;
}

ply_t* create_ply_from_edges(const int M, const int N, int* edges) {
    auto out = (ply_t*) malloc(sizeof(ply_t));
    out->type = T_PLY;
    auto ply = new communication::datatypes::YggPly();
    ply->add_element_set("face");
    ply->add_element_set("vertex");
    if (edges != nullptr) {
        ply->add_element_set("edge", edges, M, N);
    } else {
        ply->add_element_set("edge");
    }
    out->obj = ply;
    return out;
}

int ply_add_edges(ply_t* ply, int* edges, const size_t &len) {
    auto p = get_ply(ply);
    p->add_element("edge", std::vector<int>(edges, edges + len));
    return 0;
}

ply_t* create_ply_t_from_all(const int VM, const int VN, float* verts,
                             const int FM, const int FN, int* faces,
                             const int EM, const int EN, int* edges) {
    auto out = (ply_t*) malloc(sizeof(ply_t));
    if (out == nullptr)
        return nullptr;
    out->type = T_PLY;
    auto ply = new communication::datatypes::YggPly();
    if (verts != nullptr) {
        ply->add_element_set("vertex", verts, VM, VN);
    } else {
        ply->add_element_set("vertex");
    }
    if (faces != nullptr) {
        ply->add_element_set("face", faces, FM, FN);
    } else {
        ply->add_element_set("face");
    }
    if (edges != nullptr) {
        ply->add_element_set("edge", edges, EM, EN);
    } else {
        ply->add_element_set("edge");
    }
    out->obj = ply;
    return out;
}

ply_t* create_ply_t() {
    return create_ply_t_from_all(0, 0, nullptr,
                               0, 0, nullptr,
                               0, 0, nullptr);
}

void ply_get_double_array(const ply_t* ply, const char* name0, double* data, size_t &len, bool skipcolors) {
    auto p = get_ply(ply);
    if (p == nullptr)
        return;
    size_t M;
    std::vector<double> dat = p->get_double_array(name0, len, M, skipcolors);
    data = (double*)malloc(sizeof(double*) * dat.size());
    std::copy(dat.begin(), dat.end(), data);
}

void ply_get_int_array(const ply_t* ply, const char* name0, int* data, size_t &len, bool skipcolors) {
    auto p = get_ply(ply);
    if (p == nullptr)
        return;
    size_t M;
    std::vector<int> dat = p->get_int_array(name0, len, M, skipcolors);
    data = (int*)malloc(sizeof(int*) * dat.size());
    std::copy(dat.begin(), dat.end(), data);
}

bool ply_is_valid(const ply_t* ply) {
    auto p = get_ply(ply);
    if (p == nullptr)
        return false;
    return p->is_valid();
}
size_t ply_count_elements(const ply_t* ply, const char* name) {
    auto p = get_ply(ply);
    if (p == nullptr)
        return 0;
    return p->count_elements(name);
}

size_t ply_nvert(const ply_t* ply) {
    return ply_count_elements(ply, "vertex");
}
size_t ply_nedge(const ply_t* ply) {
    return ply_count_elements(ply, "edge");
}
size_t ply_nface(const ply_t* ply) {
    return ply_count_elements(ply, "face");
}