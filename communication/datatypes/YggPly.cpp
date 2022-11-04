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

YggPly::YggPly(const ply_t *ply) : YggPly() {
    const std::vector<std::string> prop_ord = {"x", "y", "z"};
    const std::map<std::string, uint16_t> prop = {{"x", rapidjson::type2flag<float>()},
                                                  {"y", rapidjson::type2flag<float>()},
                                                  {"z", rapidjson::type2flag<float>()}};

    if (ply == nullptr)
        return;
    const std::vector<std::string> colors = {"red", "green", "blue"};
    rapidjson::Ply pl;
    if (ply->nvert > 0) {
        rapidjson::PlyElementSet pes("vertex", prop_ord, colors,0., true);
        for (auto i = 0; i < ply->nvert; i++) {
            std::vector<float> vert(ply->vertices[i], ply->vertices[i] + 3);
            rapidjson::PlyElement pe(prop_ord, colors, prop, vert);
            if (ply->vertex_colors != nullptr) {
                pe.add_colors(ply->vertex_colors[i], prop, colors);
            }
            pes.add_element(pe);
        }
        pl.add_element_set("vertex", pes);
    }
    if (ply->nedge > 0) {
        rapidjson::PlyElementSet pes("edge", prop_ord, colors,0., true);
        for (auto i = 0; i < ply->nedge; i++) {
            std::vector<float> edges(ply->edges[i], ply->edges[i] + 2);
            rapidjson::PlyElement pe(prop_ord, colors, prop, edges);
            if (ply->edge_colors != nullptr) {
                pe.add_colors(ply->edge_colors[i], prop, colors);
            }
            pes.add_element(pe);
        }
        pl.add_element_set("edge", pes);
    }

    if (ply->nface > 0) {
        rapidjson::PlyElementSet pes("face", prop_ord, colors,0., true);
        for (auto i = 0; i < ply->nface; i++) {
            std::vector<float> faces(ply->faces[i], ply->faces[i] + ply->nvert_in_face[i]);
            rapidjson::PlyElement pe(prop_ord, colors, prop, faces);
            pes.add_element(pe);
        }
        pl.add_element_set("face", pes);
    }
}

ply_t* YggPly::toPlyt() {
    auto ply = (ply_t*)malloc(sizeof(ply_t));
    size_t nvert, nedge, nface;
    bool do_color_v, do_color_e;
    nvert = count_elements("vertex");
    nedge = count_elements("edge");
    nface = count_elements("face");
    rapidjson::PlyElementSet *pev = nullptr, *pee = nullptr, *pef = nullptr;
    if (nvert > 0) {
        pev = get_element_set("vertex");
        do_color_v = !pev->colors.empty();
    }
    if (nedge > 0) {
        pee = get_element_set("edge");
        do_color_e = !pee->colors.empty();
    }
    if (nface > 0) {
        pef = get_element_set("face");
    }
    alloc_ply(ply, static_cast<int>(nvert), static_cast<int>(nface), static_cast<int>(nedge), do_color_v, do_color_e);

    if (pev != nullptr) {
        int count = 0;
        for (auto &e : pev->elements) {
            auto vvec = e.get_double_array(false, 3);

            auto data = (float*)malloc(sizeof(float) * 3);
            std::copy(vvec.begin(), vvec.end(), data);
            ply->vertices[count] = data;
            if (do_color_v) {
                auto cvec = e.get_colors_array();
                auto col = (int*)malloc(sizeof(int) * 3);
                std::copy(cvec.begin(), cvec.end(), col);
                ply->vertex_colors[count] = col;
            }
            count++;
        }
    }
    if (pee != nullptr) {
        int count = 0;
        for (auto &e : pee->elements) {
            auto evec = e.get_int_array(0, false, 2);

            auto data = (int*)malloc(sizeof(int) * 2);
            std::copy(evec.begin(), evec.end(), data);
            ply->edges[count] = data;
            if (do_color_e) {
                auto cvec = e.get_colors_array();
                auto col = (int*)malloc(sizeof(int) * 3);
                std::copy(cvec.begin(), cvec.end(), col);
                ply->edge_colors[count] = col;
            }
            count++;
        }
    }
    if (pef != nullptr) {
        int count = 0;
        for (auto &e : pef->elements) {
            auto fvec = e.get_int_array(0, false, 3);

            auto data = (int*)malloc(sizeof(int) * fvec.size());
            std::copy(fvec.begin(), fvec.end(), data);
            ply->faces[count] = data;
            ply->nvert_in_face[count] = static_cast<int>(fvec.size());
            count++;
        }
    }


    return ply;
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
    for (auto &v : get_element_set("vertex")->elements) {
        std::vector<double> e = v.get_double_array(false, 3);
        printf("%s  %f, %f, %f\n", indent,
               e[0], e[1], e[2]);
    }

    printf("%s%d Edges:\n", indent, static_cast<int>(count_elements("edge")));
    for (auto &v : get_element_set("edge")->elements) {
        std::vector<int> e = v.get_int_array(3);
        printf("%s  %d, %d\n", indent,
               e[0], e[1]);
    }

    printf("%s%d Faces:\n", indent, static_cast<int>(count_elements("face")));
    for (auto &v : get_element_set("face")->elements) {
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


ply_t init_ply() {
    ply_t x;
    x.material[0] = '\0';
    x.nvert = 0;
    x.nface = 0;
    x.nedge = 0;
    x.vertices = nullptr;
    x.faces = nullptr;
    x.edges = nullptr;
    x.vertex_colors = nullptr;
    x.edge_colors = nullptr;
    x.nvert_in_face = nullptr;
    return x;
}

dtype_t* wrap_ply(ply_t* ply) {
    auto dtype = (dtype_t*)malloc(sizeof(dtype_t));
    if (dtype == nullptr)
        return nullptr;
    dtype->type = T_PLY_T;
    dtype->obj = ply;
    dtype->use_generic = false;
    return dtype;
}

void free_ply(ply_t *p) {
    int i;
    if (p->vertices != nullptr) {
        for (i = 0; i < p->nvert; i++) {
            if (p->vertices[i] != nullptr) {
                free(p->vertices[i]);
                p->vertices[i] = nullptr;
            }
        }
        free(p->vertices);
        p->vertices = nullptr;
    }
    if (p->vertex_colors != nullptr) {
        for (i = 0; i < p->nvert; i++) {
            if (p->vertex_colors[i] != nullptr) {
                free(p->vertex_colors[i]);
                p->vertex_colors[i] = nullptr;
            }
        }
        free(p->vertex_colors);
        p->vertex_colors = nullptr;
    }
    if (p->faces != nullptr) {
        for (i = 0; i < p->nface; i++) {
            if (p->faces[i] != nullptr) {
                free(p->faces[i]);
                p->faces[i] = nullptr;
            }
        }
        free(p->faces);
        p->faces = nullptr;
    }
    if (p->nvert_in_face != nullptr) {
        free(p->nvert_in_face);
        p->nvert_in_face = nullptr;
    }
    if (p->edges != nullptr) {
        for (i = 0; i < p->nedge; i++) {
            if (p->edges[i] != nullptr) {
                free(p->edges[i]);
                p->edges[i] = nullptr;
            }
        }
        free(p->edges);
        p->edges = nullptr;
    }
    if (p->edge_colors != nullptr) {
        for (i = 0; i < p->nedge; i++) {
            if (p->edge_colors[i] != nullptr) {
                free(p->edge_colors[i]);
                p->edge_colors[i] = nullptr;
            }
        }
        free(p->edge_colors);
        p->edge_colors = nullptr;
    }
}

int alloc_ply(ply_t *p, int nvert, int nface, int nedge, int do_vert_color, int do_edge_color) {
    int i;
    free_ply(p); // Ensure that existing data is freed
    p->material[0] = '\0';
    p->nvert = nvert;
    p->nface = nface;
    p->nedge = nedge;
    // Allocate vertices
    auto new_vert = (float **) malloc(p->nvert * sizeof(float *));
    if (new_vert == nullptr) {
        ygglog_error << "alloc_ply: Failed to allocate vertices.";
        free_ply(p);
        return -1;
    }
    p->vertices = new_vert;
    for (i = 0; i < p->nvert; i++) {
        auto ivert = (float *) malloc(3 * sizeof(float));
        if (ivert == nullptr) {
            ygglog_error << "alloc_ply: Failed to allocate vertex " << i << ".";
            free_ply(p);
            return -1;
        }
        p->vertices[i] = ivert;
    }
    ygglog_debug << "alloc_ply: Allocated " << nvert << " vertices.";
    // Allocate vertex colors
    if (do_vert_color) {
        int **new_vert_colors = (int **) malloc(p->nvert * sizeof(int *));
        if (new_vert_colors == nullptr) {
            ygglog_error << "alloc_ply: Failed to allocate vertex_colors.";
            free_ply(p);
            return -1;
        }
        p->vertex_colors = new_vert_colors;
        for (i = 0; i < p->nvert; i++) {
            int *ivert = (int *) malloc(3 * sizeof(int));
            if (ivert == nullptr) {
                ygglog_error << "alloc_ply: Failed to allocate vertex color " << i << ".";
                free_ply(p);
                return -1;
            }
            p->vertex_colors[i] = ivert;
        }
        ygglog_debug << "alloc_ply: Allocated " << nvert << " vertex colors.";
    }
    // Allocate faces
    int **new_face = (int **) malloc(p->nface * sizeof(int *));
    if (new_face == nullptr) {
        ygglog_error << "alloc_ply: Failed to allocate faces.";
        free_ply(p);
        return -1;
    }
    p->faces = new_face;
    for (i = 0; i < p->nface; i++) {
        p->faces[i] = nullptr;
        /* int *iface = (int*)malloc(3*sizeof(int)); */
        /* if (iface == nullptr) { */
        /*   ygglog_error("alloc_ply: Failed to allocate face %d.", i); */
        /*   free_ply(p); */
        /*   return -1; */
        /* } */
    }
    ygglog_debug << "alloc_ply: Allocated " << nface << " faces.";
    // Allocate nvert_in_face
    int *new_nvert = (int *) malloc(p->nface * sizeof(int));
    if (new_nvert == nullptr) {
        ygglog_error << "alloc_ply: Failed to allocate nvert_in_face.";
        free_ply(p);
        return -1;
    }
    p->nvert_in_face = new_nvert;
    for (i = 0; i < p->nface; i++) {
        p->nvert_in_face[i] = 0;
    }
    // Allocate edges
    int **new_edge = (int **) malloc(p->nedge * sizeof(int *));
    if (new_edge == nullptr) {
        ygglog_error << "alloc_ply: Failed to allocate edges.";
        free_ply(p);
        return -1;
    }
    p->edges = new_edge;
    for (i = 0; i < p->nedge; i++) {
        int *iedge = (int *) malloc(2 * sizeof(int));
        if (iedge == nullptr) {
            ygglog_error << "alloc_ply: Failed to allocate edge " << i << ".";
            free_ply(p);
            return -1;
        }
        p->edges[i] = iedge;
    }
    ygglog_debug << "alloc_ply: Allocated " << nedge << " edges.";
    // Allocate edge colors
    if (do_edge_color) {
        int **new_edge_colors = (int **) malloc(p->nedge * sizeof(int *));
        if (new_edge_colors == nullptr) {
            ygglog_error << "alloc_ply: Failed to allocate edge_colors.";
            free_ply(p);
            return -1;
        }
        p->edge_colors = new_edge_colors;
        for (i = 0; i < p->nedge; i++) {
            int *iedge = (int *) malloc(3 * sizeof(int));
            if (iedge == nullptr) {
                ygglog_error << "alloc_ply: Failed to allocate edge color " << i << ".";
                free_ply(p);
                return -1;
            }
            p->edge_colors[i] = iedge;
        }
        ygglog_debug << "alloc_ply: Allocated " << nedge << " edge colors.";
    }
    ygglog_debug << "alloc_ply: Allocate for " << p->nvert << " vertices, " << p->nface << " faces, and " << p->nedge << " edges.";
    return 0;
}

ply_t copy_ply(ply_t src) {
    int i;
    int do_vert_color = 0, do_edge_color = 0;
    if (src.vertex_colors != nullptr) {
        do_vert_color = 1;
    }
    if (src.edge_colors != nullptr) {
        do_edge_color = 1;
    }
    ply_t out = init_ply();
    alloc_ply(&out, src.nvert, src.nface, src.nedge, do_vert_color, do_edge_color);
    strcpy(out.material, src.material);
    // Copy vertices
    for (i = 0; i < src.nvert; i++) {
        memcpy(out.vertices[i], src.vertices[i], 3 * sizeof(float));
    }
    if (do_vert_color) {
        for (i = 0; i < src.nvert; i++) {
            memcpy(out.vertex_colors[i], src.vertex_colors[i], 3 * sizeof(int));
        }
    }
    // Copy faces
    memcpy(out.nvert_in_face, src.nvert_in_face, src.nface * sizeof(int));
    for (i = 0; i < src.nface; i++) {
        int *iface = (int *) realloc(out.faces[i], src.nvert_in_face[i] * sizeof(int));
        if (iface == nullptr) {
            ygglog_debug << "PlyDict::copy_ply: Could not allocate face " << i << ".";
            free_ply(&out);
            return out;
        }
        out.faces[i] = iface;
        memcpy(out.faces[i], src.faces[i], src.nvert_in_face[i] * sizeof(int));
    }
    // Copy edges
    for (i = 0; i < src.nedge; i++) {
        memcpy(out.edges[i], src.edges[i], 2 * sizeof(int));
    }
    if (do_edge_color) {
        for (i = 0; i < src.nedge; i++) {
            memcpy(out.edge_colors[i], src.edge_colors[i], 3 * sizeof(int));
        }
    }
    return out;
}

void display_ply_indent(ply_t p, const char *indent) {
    int i, j;
    printf("%s%d Vertices:\n", indent, p.nvert);
    for (i = 0; i < p.nvert; i++) {
        printf("%s  %f, %f, %f\n", indent,
               p.vertices[i][0], p.vertices[i][1], p.vertices[i][2]);
    }
    printf("%s%d Edges:\n", indent, p.nedge);
    for (i = 0; i < p.nedge; i++) {
        printf("%s  %d, %d\n", indent,
               p.edges[i][0], p.edges[i][1]);
    }
    printf("%s%d Faces:\n", indent, p.nface);
    for (i = 0; i < p.nface; i++) {
        printf("%s  %d", indent, p.faces[i][0]);
        for (j = 1; j < p.nvert_in_face[i]; j++)
            printf(", %d", p.faces[i][j]);
        printf("\n");
    }
}

void display_ply(ply_t p) {
    display_ply_indent(p, "");
}