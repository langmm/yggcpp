#include <map>
#include "YggObj.hpp"
#include "Obj.h"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
YggObj::YggObj() : rapidjson::ObjWavefront() {

}

YggObj::YggObj(const YggObj *src) : rapidjson::ObjWavefront(*(static_cast<const rapidjson::ObjWavefront *>(src))) {

}

YggObj::YggObj(const obj_t *obj) : rapidjson::ObjWavefront() {
    if (obj == nullptr)
        return;
    int i;
    std::map<int, rapidjson::ObjRef> vertmap;
    for (i = 0; i < obj->nvert; i++) {
        auto vert = new rapidjson::ObjVertex();
        vert->assign_values(vert->values, obj->vertices[i], 4);
        vert->from_values();
        vert->add_colors(obj->vertex_colors[i], 3);
        vert = dynamic_cast<rapidjson::ObjVertex *>(add_element(vert));
        vertmap[i] = static_cast<rapidjson::ObjRef>(elements.size() - 1);
    }
    std::map<int, rapidjson::ObjRef> texmap;
    for (i = 0; i < obj->ntexc; i++) {
        auto texture = new rapidjson::ObjVTexture();
        texture->assign_values(texture->values, obj->texcoords[i], 3);
        texture->from_values();
        texture = dynamic_cast<rapidjson::ObjVTexture *>(add_element(texture));
        texmap[i] = static_cast<rapidjson::ObjRef>(elements.size() - 1);
    }
    std::map<int, rapidjson::ObjRef> normmap;
    for (i = 0; i < obj->nnorm; i++) {
        auto normal = new rapidjson::ObjVNormal();
        normal->assign_values(normal->values, obj->normals[i], 3);
        normal->from_values();
        normal = dynamic_cast<rapidjson::ObjVNormal *>(add_element(normal));
        normmap[i] = static_cast<rapidjson::ObjRef>(elements.size() - 1);
    }
    for (i = 0; i < obj->nparam; i++) {
        auto param = new rapidjson::ObjVParameter();
        param->assign_values(param->values, obj->params[i], 3);
        param->from_values();
        param = dynamic_cast<rapidjson::ObjVParameter *>(add_element(param));
    }
    for (i = 0; i < obj->npoint; i++) {
        std::vector<rapidjson::ObjRef> refs;
        for (int j = 0; j < obj->nvert_in_point[i]; j++) {
            refs.push_back(vertmap[obj->points[i][j]]);
        }
        auto point = new rapidjson::ObjPoint();
        point->assign_values(point->values, refs);
        point->from_values();
        point = dynamic_cast<rapidjson::ObjPoint *>(add_element(point));
    }
    for (i = 0; i < obj->nline; i++) {
        std::vector<rapidjson::ObjRefVertex> refs;
        for (int j = 0; j < obj->nvert_in_line[i]; j++) {
            rapidjson::ObjRefVertex orv(vertmap[obj->lines[i][j]], texmap[obj->line_texcoords[i][j]]);
            refs.push_back(orv);
        }
        auto line = new rapidjson::ObjLine();
        line->assign_values(line->values, refs);
        line->from_values();
        line = dynamic_cast<rapidjson::ObjLine *>(add_element(line));
    }
    for (i = 0; i < obj->nface; i++) {
        std::vector<rapidjson::ObjRefVertex> refs;
        for (int j = 0; j < obj->nvert_in_face[i]; j++) {
            rapidjson::ObjRefVertex orv(vertmap[obj->faces[i][j]], texmap[obj->face_texcoords[i][j]],
                                        normmap[obj->face_normals[i][j]]);
            refs.push_back(orv);
        }
        auto face = new rapidjson::ObjFace();
        face->assign_values(face->values, refs);
        face->from_values();
        face = dynamic_cast<rapidjson::ObjFace *>(add_element(face));
    }
    for (i = 0; i < obj->ncurve; i++) {
        std::vector<rapidjson::ObjRef> refs;
        for (int j = 0; j < obj->nvert_in_curve[i]; j++) {
            refs.push_back(vertmap[obj->curves[i][j]]);
        }
        auto curve = new rapidjson::ObjCurve(obj->curve_params[i][0], obj->curve_params[i][1], refs);
        //curve->assign_values(curve->values, refs);
        //curve->from_values();
        curve = dynamic_cast<rapidjson::ObjCurve *>(add_element(curve));
    }
    for (i = 0; i < obj->ncurve2; i++) {
        std::vector<rapidjson::ObjRef> refs;
        for (int j = 0; j < obj->nparam_in_curve2[i]; j++) {
            refs.push_back(vertmap[obj->curves2[i][j]]);
        }
        auto curve = new rapidjson::ObjCurve2D();
        curve->assign_values(curve->values, refs);
        curve->from_values();
        curve = dynamic_cast<rapidjson::ObjCurve2D *>(add_element(curve));
    }
    for (i = 0; i < obj->nsurf; i++) {
        std::vector<rapidjson::ObjRefVertex> refs;
        for (int j = 0; j < obj->nvert_in_surface[i]; j++) {
            rapidjson::ObjRefVertex orv(vertmap[obj->surfaces[i][j]], texmap[obj->surface_texcoords[i][j]],
                                        normmap[obj->surface_normals[i][j]]);
            refs.push_back(orv);
        }
        auto sface = new rapidjson::ObjSurface(obj->surface_params_u[i][0], obj->surface_params_u[i][1],
                                               obj->surface_params_v[i][0], obj->surface_params_v[i][1],
                                               refs);
        sface = dynamic_cast<rapidjson::ObjSurface *>(add_element(sface));
    }

}

YggObj::~YggObj() noexcept {

}

obj_t *YggObj::toObjt() {
    auto obj = (obj_t *) malloc(sizeof(obj_t));
    auto listing = element_listing();
    int nvert = static_cast<int>(listing.count("v"));
    int ntexc = static_cast<int>(listing.count("vt"));
    int nnorm = static_cast<int>(listing.count("vn"));
    int nparam = static_cast<int>(listing.count("vp"));
    int npoint = static_cast<int>(listing.count("p"));
    int nline = static_cast<int>(listing.count("l"));
    int nface = static_cast<int>(listing.count("f"));
    int ncurve = static_cast<int>(listing.count("curv"));
    int ncurve2 = static_cast<int>(listing.count("curv2"));
    int nsurf = static_cast<int>(listing.count("surf"));
    alloc_obj(obj, nvert, ntexc, nnorm, nparam, npoint, nline, nface, ncurve, ncurve2, nsurf, true);
    int i;
    std::map<rapidjson::ObjRef, int> vertmap;
    for (i = 0; i < nvert; i++) {
        vertmap[i] = static_cast<int>(listing["v"][i]);
        auto vert = dynamic_cast<rapidjson::ObjVertex *>(elements[listing["v"][i]]);
        std::vector<uint8_t> colors;
        vert->get_colors_array(colors);
        std::vector<float> coords;
        vert->get_properties(coords, true);
        std::copy(coords.begin(), coords.end(), obj->vertices[i]);
        std::copy(colors.begin(), colors.end(), obj->vertex_colors[i]);
    }
    std::map<rapidjson::ObjRef, int> texmap;
    for (i = 0; i < ntexc; i++) {
        texmap[i] = static_cast<int>(listing["vt"][i]);
        auto vert = dynamic_cast<rapidjson::ObjVTexture *>(elements[listing["vt"][i]]);
        std::vector<float> coords;
        vert->get_properties(coords, true);
        std::copy(coords.begin(), coords.end(), obj->texcoords[i]);
    }
    std::map<rapidjson::ObjRef, int> normmap;
    for (i = 0; i < nnorm; i++) {
        normmap[i] = static_cast<int>(listing["vn"][i]);
        auto norm = dynamic_cast<rapidjson::ObjVTexture *>(elements[listing["vn"][i]]);
        std::vector<float> coords;
        norm->get_properties(coords, true);
        std::copy(coords.begin(), coords.end(), obj->normals[i]);
    }
    for (i = 0; i < nparam; i++) {
        auto param = dynamic_cast<rapidjson::ObjVParameter *>(elements[listing["vt"][i]]);
        std::vector<float> coords;
        param->get_properties(coords, true);
        std::copy(coords.begin(), coords.end(), obj->params[i]);
    }
    for (i = 0; i < npoint; i++) {
        auto point = dynamic_cast<rapidjson::ObjPoint *>(elements[listing["vt"][i]]);
        auto points = point->values;
        obj->nvert_in_point[i] = static_cast<int>(points.size());
        auto cpnts = (int *) malloc(sizeof(int) * points.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        std::copy(points.begin(), points.end(), cpnts);
        obj->points[i] = cpnts;
    }
    for (i = 0; i < nline; i++) {
        auto line = dynamic_cast<rapidjson::ObjLine *>(elements[listing["l"][i]]);
        auto refs = line->values;
        obj->nvert_in_line[i] = static_cast<int>(refs.size());
        auto cpnts = (int *) malloc(sizeof(int) * refs.size());
        auto ctex = (int *) malloc(sizeof(int) * refs.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        if (ctex == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        for (auto j = 0; j < refs.size(); j++) {
            cpnts[j] = vertmap[refs[j].v];
            ctex[j] = texmap[refs[j].vt];
        }
        obj->lines[i] = cpnts;
        obj->line_texcoords[i] = ctex;
    }
    for (i = 0; i < nface; i++) {
        auto face = dynamic_cast<rapidjson::ObjFace *>(elements[listing["f"][i]]);
        auto refs = face->values;
        obj->nvert_in_face[i] = static_cast<int>(refs.size());
        auto cpnts = (int *) malloc(sizeof(int) * refs.size());
        auto ctex = (int *) malloc(sizeof(int) * refs.size());
        auto cnorm = (int *) malloc(sizeof(int) * refs.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        if (ctex == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        if (cnorm == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }

        for (auto j = 0; j < refs.size(); j++) {
            cpnts[j] = vertmap[refs[j].v];
            ctex[j] = texmap[refs[j].vt];
            cnorm[j] = normmap[refs[j].vn];
        }
        obj->faces[i] = cpnts;
        obj->face_texcoords[i] = ctex;
        obj->face_normals[i] = cnorm;
    }
    for (i = 0; i < ncurve; i++) {
        auto curve = dynamic_cast<rapidjson::ObjCurve *>(elements[listing["curv"][i]]);
        auto refs = curve->values;
        obj->nvert_in_curve[i] = static_cast<int>(refs.size());
        auto cpnts = (int *) malloc(sizeof(int) * refs.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        for (auto j = 0; j < refs.size(); j++) {
            cpnts[j] = vertmap[refs[j]];
        }
        obj->curves[i] = cpnts;
        obj->curve_params[i][0] = static_cast<float>(curve->u0);
        obj->curve_params[i][1] = static_cast<float>(curve->u1);
    }
    for (i = 0; i < ncurve2; i++) {
        auto curve = dynamic_cast<rapidjson::ObjCurve2D *>(elements[listing["curv2"][i]]);
        auto refs = curve->values;
        obj->nvert_in_curve[i] = static_cast<int>(refs.size());
        auto cpnts = (int *) malloc(sizeof(int) * refs.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        for (auto j = 0; j < refs.size(); j++) {
            cpnts[j] = vertmap[refs[j]];
        }
        obj->curves2[i] = cpnts;
    }
    for (i = 0; i < nsurf; i++) {
        auto surface = dynamic_cast<rapidjson::ObjSurface *>(elements[listing["surf"][i]]);
        auto refs = surface->values;
        obj->nvert_in_face[i] = static_cast<int>(refs.size());
        auto cpnts = (int *) malloc(sizeof(int) * refs.size());
        auto ctex = (int *) malloc(sizeof(int) * refs.size());
        auto cnorm = (int *) malloc(sizeof(int) * refs.size());
        if (cpnts == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        if (ctex == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }
        if (cnorm == nullptr) {
            utils::ygglog_error("Failed to alloc");
            free_obj(obj);
            return nullptr;
        }

        for (auto j = 0; j < refs.size(); j++) {
            cpnts[j] = vertmap[refs[j].v];
            ctex[j] = texmap[refs[j].vt];
            cnorm[j] = normmap[refs[j].vn];
        }
        obj->surfaces[i] = cpnts;
        obj->surface_texcoords[i] = ctex;
        obj->surface_normals[i] = cnorm;
        obj->surface_params_u[i][0] = static_cast<float>(surface->s0);
        obj->surface_params_u[i][1] = static_cast<float>(surface->s1);
        obj->surface_params_v[i][0] = static_cast<float>(surface->t0);
        obj->surface_params_v[i][1] = static_cast<float>(surface->t1);
    }
    return obj;
}
}
}
dtype_t* wrap_obj(obj_t* obj) {
    auto dtype = (dtype_t*)malloc(sizeof(dtype_t));
    if (dtype == nullptr)
        return nullptr;
    dtype->type = T_OBJ_T;
    dtype->obj = obj;
    dtype->use_generic = false;
    return dtype;

}
obj_t init_obj() {
    obj_t x;
    x.material[0] = '\0';
    x.nvert = 0;
    x.ntexc = 0;
    x.nnorm = 0;
    x.nparam = 0;
    x.npoint = 0;
    x.nline = 0;
    x.nface = 0;
    x.ncurve = 0;
    x.ncurve2 = 0;
    x.nsurf = 0;
    x.vertices = nullptr;
    x.vertex_colors = nullptr;
    x.texcoords = nullptr;
    x.normals = nullptr;
    x.params = nullptr;
    x.points = nullptr;
    x.nvert_in_point = nullptr;
    x.lines = nullptr;
    x.nvert_in_line = nullptr;
    x.line_texcoords = nullptr;
    x.faces = nullptr;
    x.nvert_in_face = nullptr;
    x.face_texcoords = nullptr;
    x.face_normals = nullptr;
    x.curves = nullptr;
    x.curve_params = nullptr;
    x.nvert_in_curve = nullptr;
    x.curves2 = nullptr;
    x.nparam_in_curve2 = nullptr;
    x.surfaces = nullptr;
    x.nvert_in_surface = nullptr;
    x.surface_params_u = nullptr;
    x.surface_params_v = nullptr;
    x.surface_texcoords = nullptr;
    x.surface_normals = nullptr;
    return x;
}

void free_obj(obj_t *p) {
    int i;
    // Vertices
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
    // Texcoords
    if (p->texcoords != nullptr) {
        for (i = 0; i < p->ntexc; i++) {
            if (p->texcoords[i] != nullptr) {
                free(p->texcoords[i]);
                p->texcoords[i] = nullptr;
            }
        }
        free(p->texcoords);
        p->texcoords = nullptr;
    }
    // Normals
    if (p->normals != nullptr) {
        for (i = 0; i < p->nnorm; i++) {
            if (p->normals[i] != nullptr) {
                free(p->normals[i]);
                p->normals[i] = nullptr;
            }
        }
        free(p->normals);
        p->normals = nullptr;
    }
    // Parameters
    if (p->params != nullptr) {
        for (i = 0; i < p->nparam; i++) {
            if (p->params[i] != nullptr) {
                free(p->params[i]);
                p->params[i] = nullptr;
            }
        }
        free(p->params);
        p->params = nullptr;
    }
    // Points
    if (p->points != nullptr) {
        for (i = 0; i < p->npoint; i++) {
            if (p->points[i] != nullptr) {
                free(p->points[i]);
                p->points[i] = nullptr;
            }
        }
        free(p->points);
        p->points = nullptr;
    }
    if (p->nvert_in_point != nullptr) {
        free(p->nvert_in_point);
        p->nvert_in_point = nullptr;
    }
    // Lines
    if (p->lines != nullptr) {
        for (i = 0; i < p->nline; i++) {
            if (p->lines[i] != nullptr) {
                free(p->lines[i]);
                p->lines[i] = nullptr;
            }
        }
        free(p->lines);
        p->lines = nullptr;
    }
    if (p->nvert_in_line != nullptr) {
        free(p->nvert_in_line);
        p->nvert_in_line = nullptr;
    }
    if (p->line_texcoords != nullptr) {
        for (i = 0; i < p->nline; i++) {
            if (p->line_texcoords[i] != nullptr) {
                free(p->line_texcoords[i]);
                p->line_texcoords[i] = nullptr;
            }
        }
        free(p->line_texcoords);
        p->line_texcoords = nullptr;
    }
    // Faces
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
    if (p->face_texcoords != nullptr) {
        for (i = 0; i < p->nface; i++) {
            if (p->face_texcoords[i] != nullptr) {
                free(p->face_texcoords[i]);
                p->face_texcoords[i] = nullptr;
            }
        }
        free(p->face_texcoords);
        p->face_texcoords = nullptr;
    }
    if (p->face_normals != nullptr) {
        for (i = 0; i < p->nface; i++) {
            if (p->face_normals[i] != nullptr) {
                free(p->face_normals[i]);
                p->face_normals[i] = nullptr;
            }
        }
        free(p->face_normals);
        p->face_normals = nullptr;
    }
    // Curves
    if (p->curves != nullptr) {
        for (i = 0; i < p->ncurve; i++) {
            if (p->curves[i] != nullptr) {
                free(p->curves[i]);
                p->curves[i] = nullptr;
            }
        }
        free(p->curves);
        p->curves = nullptr;
    }
    if (p->curve_params != nullptr) {
        for (i = 0; i < p->ncurve; i++) {
            if (p->curve_params[i] != nullptr) {
                free(p->curve_params[i]);
                p->curve_params[i] = nullptr;
            }
        }
        free(p->curve_params);
        p->curve_params = nullptr;
    }
    if (p->nvert_in_curve != nullptr) {
        free(p->nvert_in_curve);
        p->nvert_in_curve = nullptr;
    }
    // Curves2
    if (p->curves2 != nullptr) {
        for (i = 0; i < p->ncurve2; i++) {
            if (p->curves2[i] != nullptr) {
                free(p->curves2[i]);
                p->curves2[i] = nullptr;
            }
        }
        free(p->curves2);
        p->curves2 = nullptr;
    }
    if (p->nparam_in_curve2 != nullptr) {
        free(p->nparam_in_curve2);
        p->nparam_in_curve2 = nullptr;
    }
    // Surfaces
    if (p->surfaces != nullptr) {
        for (i = 0; i < p->nsurf; i++) {
            if (p->surfaces[i] != nullptr) {
                free(p->surfaces[i]);
                p->surfaces[i] = nullptr;
            }
        }
        free(p->surfaces);
        p->surfaces = nullptr;
    }
    if (p->nvert_in_surface != nullptr) {
        free(p->nvert_in_surface);
        p->nvert_in_surface = nullptr;
    }
    if (p->surface_params_u != nullptr) {
        for (i = 0; i < p->nsurf; i++) {
            if (p->surface_params_u[i] != nullptr) {
                free(p->surface_params_u[i]);
                p->surface_params_u[i] = nullptr;
            }
        }
        free(p->surface_params_u);
        p->surface_params_u = nullptr;
    }
    if (p->surface_params_v != nullptr) {
        for (i = 0; i < p->nsurf; i++) {
            if (p->surface_params_v[i] != nullptr) {
                free(p->surface_params_v[i]);
                p->surface_params_v[i] = nullptr;
            }
        }
        free(p->surface_params_v);
        p->surface_params_v = nullptr;
    }
    if (p->surface_texcoords != nullptr) {
        for (i = 0; i < p->nsurf; i++) {
            if (p->surface_texcoords[i] != nullptr) {
                free(p->surface_texcoords[i]);
                p->surface_texcoords[i] = nullptr;
            }
        }
        free(p->surface_texcoords);
        p->surface_texcoords = nullptr;
    }
    if (p->surface_normals != nullptr) {
        for (i = 0; i < p->nsurf; i++) {
            if (p->surface_normals[i] != nullptr) {
                free(p->surface_normals[i]);
                p->surface_normals[i] = nullptr;
            }
        }
        free(p->surface_normals);
        p->surface_normals = nullptr;
    }
    // Counts
    p->material[0] = '\0';
    p->nvert = 0;
    p->ntexc = 0;
    p->nnorm = 0;
    p->nparam = 0;
    p->npoint = 0;
    p->nline = 0;
    p->nface = 0;
    p->ncurve = 0;
    p->ncurve2 = 0;
    p->nsurf = 0;
}

int alloc_obj(obj_t *p, int nvert, int ntexc, int nnorm, int nparam,
              int npoint, int nline, int nface, int ncurve, int ncurve2,
              int nsurf, int do_color) {
    int i;
    free_obj(p); // Ensure that existing data is freed
    p->nvert = nvert;
    p->ntexc = ntexc;
    p->nnorm = nnorm;
    p->nparam = nparam;
    p->npoint = npoint;
    p->nline = nline;
    p->nface = nface;
    p->ncurve = ncurve;
    p->ncurve2 = ncurve2;
    p->nsurf = nsurf;
    // Allocate vertices
    if (nvert > 0) {
        auto new_vert = (float **) malloc(p->nvert * sizeof(float *));
        if (new_vert == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate vertices.");
            free_obj(p);
            return -1;
        }
        p->vertices = new_vert;
        for (i = 0; i < p->nvert; i++) {
            auto ivert = (float *) malloc(4 * sizeof(float));
            if (ivert == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate vertex %d.", i);
                free_obj(p);
                return -1;
            }
            p->vertices[i] = ivert;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d vertices.", nvert);
    }
    // Allocate vertex colors
    if ((nvert > 0) && (do_color)) {
        int **new_vert_colors = (int **) malloc(p->nvert * sizeof(int *));
        if (new_vert_colors == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate vertex_colors.");
            free_obj(p);
            return -1;
        }
        p->vertex_colors = new_vert_colors;
        for (i = 0; i < p->nvert; i++) {
            int *ivert = (int *) malloc(3 * sizeof(int));
            if (ivert == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate vertex color %d.", i);
                free_obj(p);
                return -1;
            }
            p->vertex_colors[i] = ivert;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d vertex colors.", nvert);
    }
    // Allocate texcoords
    if (p->ntexc > 0) {
        auto new_texc = (float **) malloc(p->ntexc * sizeof(float *));
        if (new_texc == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate texcoords.");
            free_obj(p);
            return -1;
        }
        p->texcoords = new_texc;
        for (i = 0; i < p->ntexc; i++) {
            auto itexc = (float *) malloc(3 * sizeof(float));
            if (itexc == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate texcoord %d.", i);
                free_obj(p);
                return -1;
            }
            p->texcoords[i] = itexc;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d texcoords.", ntexc);
    }
    // Allocate normals
    if (p->nnorm > 0) {
        auto new_norm = (float **) malloc(p->nnorm * sizeof(float *));
        if (new_norm == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate normals.");
            free_obj(p);
            return -1;
        }
        p->normals = new_norm;
        for (i = 0; i < p->nnorm; i++) {
            auto inorm = (float *) malloc(3 * sizeof(float));
            if (inorm == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate normal %d.", i);
                free_obj(p);
                return -1;
            }
            p->normals[i] = inorm;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d normals.", nnorm);
    }
    // Allocate parameters
    if (p->nparam > 0) {
        auto new_param = (float **) malloc(p->nparam * sizeof(float *));
        if (new_param == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate params.");
            free_obj(p);
            return -1;
        }
        p->params = new_param;
        for (i = 0; i < p->nparam; i++) {
            auto iparam = (float *) malloc(3 * sizeof(float));
            if (iparam == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate param %d.", i);
                free_obj(p);
                return -1;
            }
            p->params[i] = iparam;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d params.", nparam);
    }
    // Allocate points
    if (p->npoint > 0) {
        auto new_point = (int **) malloc(p->npoint * sizeof(int *));
        if (new_point == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate points.");
            free_obj(p);
            return -1;
        }
        p->points = new_point;
        for (i = 0; i < p->npoint; i++) {
            p->points[i] = nullptr;
        }
        int *new_nvert_point = (int *) malloc(p->npoint * sizeof(int));
        if (new_nvert_point == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nvert_in_point.");
            free_obj(p);
            return -1;
        }
        p->nvert_in_point = new_nvert_point;
        for (i = 0; i < p->npoint; i++) {
            p->nvert_in_point[i] = 0;
        }
    }
    // Allocate lines
    if (p->nline > 0) {
        auto new_line = (int **) malloc(p->nline * sizeof(int *));
        if (new_line == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate lines.");
            free_obj(p);
            return -1;
        }
        p->lines = new_line;
        for (i = 0; i < p->nline; i++) {
            p->lines[i] = nullptr;
        }
        auto new_nvert_line = (int *) malloc(p->nline * sizeof(int));
        if (new_nvert_line == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nvert_in_line.");
            free_obj(p);
            return -1;
        }
        p->nvert_in_line = new_nvert_line;
        for (i = 0; i < p->nline; i++) {
            p->nvert_in_line[i] = 0;
        }
        auto new_line_texcoords = (int **) malloc(p->nline * sizeof(int *));
        if (new_line_texcoords == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate line_texcoords.");
            free_obj(p);
            return -1;
        }
        p->line_texcoords = new_line_texcoords;
        for (i = 0; i < p->nline; i++) {
            p->line_texcoords[i] = nullptr;
        }
    }
    // Allocate faces
    if (p->nface > 0) {
        auto new_face = (int **) malloc(p->nface * sizeof(int *));
        if (new_face == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate faces.");
            free_obj(p);
            return -1;
        }
        p->faces = new_face;
        for (i = 0; i < p->nface; i++) {
            p->faces[i] = nullptr;
        }
        auto new_nvert_face = (int *) malloc(p->nface * sizeof(int));
        if (new_nvert_face == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nvert_in_face.");
            free_obj(p);
            return -1;
        }
        p->nvert_in_face = new_nvert_face;
        for (i = 0; i < p->nface; i++) {
            p->nvert_in_face[i] = 0;
        }
        auto new_face_texcoords = (int **) malloc(p->nface * sizeof(int *));
        if (new_face_texcoords == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate face_texcoords.");
            free_obj(p);
            return -1;
        }
        p->face_texcoords = new_face_texcoords;
        for (i = 0; i < p->nface; i++) {
            p->face_texcoords[i] = nullptr;
        }
        auto new_face_normals = (int **) malloc(p->nface * sizeof(int *));
        if (new_face_normals == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate face_normals.");
            free_obj(p);
            return -1;
        }
        p->face_normals = new_face_normals;
        for (i = 0; i < p->nface; i++) {
            p->face_normals[i] = nullptr;
        }
        communication::utils::ygglog_debug("alloc_obj: Allocated %d faces.", nface);
    }
    // Allocate curves
    if (p->ncurve > 0) {
        auto new_curve = (int **) malloc(p->ncurve * sizeof(int *));
        if (new_curve == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate curves.");
            free_obj(p);
            return -1;
        }
        p->curves = new_curve;
        for (i = 0; i < p->ncurve; i++) {
            p->curves[i] = nullptr;
        }
        auto new_nvert_curve = (int *) malloc(p->ncurve * sizeof(int));
        if (new_nvert_curve == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nvert_in_curve.");
            free_obj(p);
            return -1;
        }
        p->nvert_in_curve = new_nvert_curve;
        for (i = 0; i < p->ncurve; i++) {
            p->nvert_in_curve[i] = 0;
        }
        auto new_curve_params = (float **) malloc(p->ncurve * sizeof(float *));
        if (new_curve_params == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate curve_params.");
            free_obj(p);
            return -1;
        }
        p->curve_params = new_curve_params;
        for (i = 0; i < p->ncurve; i++) {
            auto iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate curve param %d.", i);
                free_obj(p);
                return -1;
            }
            p->curve_params[i] = iparam;
        }
    }
    // Curves2
    if (p->ncurve2 > 0) {
        auto new_curve2 = (int **) malloc(p->ncurve2 * sizeof(int *));
        if (new_curve2 == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate curves2.");
            free_obj(p);
            return -1;
        }
        p->curves2 = new_curve2;
        for (i = 0; i < p->ncurve2; i++) {
            p->curves2[i] = nullptr;
        }
        int *new_nparam_curve2 = (int *) malloc(p->ncurve2 * sizeof(int));
        if (new_nparam_curve2 == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nparam_in_curve2.");
            free_obj(p);
            return -1;
        }
        p->nparam_in_curve2 = new_nparam_curve2;
        for (i = 0; i < p->ncurve2; i++) {
            p->nparam_in_curve2[i] = 0;
        }
    }
    // Surfaces
    if (p->nsurf > 0) {
        auto new_surface = (int **) malloc(p->nsurf * sizeof(int *));
        if (new_surface == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate surfaces.");
            free_obj(p);
            return -1;
        }
        p->surfaces = new_surface;
        for (i = 0; i < p->nsurf; i++) {
            p->surfaces[i] = nullptr;
        }
        auto new_nvert_surface = (int *) malloc(p->nsurf * sizeof(int));
        if (new_nvert_surface == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate nvert_in_surface.");
            free_obj(p);
            return -1;
        }
        p->nvert_in_surface = new_nvert_surface;
        for (i = 0; i < p->nsurf; i++) {
            p->nvert_in_surface[i] = 0;
        }
        auto new_surface_params_u = (float **) malloc(p->nsurf * sizeof(float *));
        if (new_surface_params_u == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate surface_params_u.");
            free_obj(p);
            return -1;
        }
        p->surface_params_u = new_surface_params_u;
        for (i = 0; i < p->nsurf; i++) {
            auto iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate surface param %d.", i);
                free_obj(p);
                return -1;
            }
            p->surface_params_u[i] = iparam;
        }
        auto new_surface_params_v = (float **) malloc(p->nsurf * sizeof(float *));
        if (new_surface_params_v == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate surface_params_v.");
            free_obj(p);
            return -1;
        }
        p->surface_params_v = new_surface_params_v;
        for (i = 0; i < p->nsurf; i++) {
            auto iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                communication::utils::ygglog_error("alloc_obj: Failed to allocate surface param %d.", i);
                free_obj(p);
                return -1;
            }
            p->surface_params_v[i] = iparam;
        }
        auto new_surface_texcoords = (int **) malloc(p->nsurf * sizeof(int *));
        if (new_surface_texcoords == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate surface_texcoords.");
            free_obj(p);
            return -1;
        }
        p->surface_texcoords = new_surface_texcoords;
        for (i = 0; i < p->nsurf; i++) {
            p->surface_texcoords[i] = nullptr;
        }
        auto new_surface_normals = (int **) malloc(p->nsurf * sizeof(int *));
        if (new_surface_normals == nullptr) {
            communication::utils::ygglog_error("alloc_obj: Failed to allocate surface_normals.");
            free_obj(p);
            return -1;
        }
        p->surface_normals = new_surface_normals;
        for (i = 0; i < p->nsurf; i++) {
            p->surface_normals[i] = nullptr;
        }
    }
    // Return
    communication::utils::ygglog_debug("alloc_obj: Allocated for\n"
                                       "\t%d vertices,\n"
                                       "\t%d texture coordinates,\n"
                                       "\t%d normals,\n"
                                       "\t%d parameters,\n"
                                       "\t%d points,\n"
                                       "\t%d lines,\n"
                                       "\t%d faces,\n"
                                       "\t%d curves,\n"
                                       "\t%d curve2, and\n"
                                       "\t%d surfaces.\n",
                                       p->nvert, p->ntexc, p->nnorm, p->nparam, p->npoint,
                                       p->nline, p->nface, p->ncurve, p->ncurve2, p->nsurf);
    return 0;
}

obj_t copy_obj(obj_t src) {
    int i;
    int do_color = 0;
    if (src.vertex_colors != nullptr) {
        do_color = 1;
    }
    obj_t dst = init_obj();
    alloc_obj(&dst, src.nvert, src.ntexc, src.nnorm, src.nparam,
              src.npoint, src.nline, src.nface, src.ncurve, src.ncurve2,
              src.nsurf, do_color);
    strcpy(dst.material, src.material);
    // Copy vertices
    for (i = 0; i < dst.nvert; i++) {
        memcpy(dst.vertices[i], src.vertices[i], 4 * sizeof(float));
    }
    // Copy vertex colors
    if (do_color) {
        for (i = 0; i < dst.nvert; i++) {
            memcpy(dst.vertex_colors[i], src.vertex_colors[i], 3 * sizeof(int));
        }
    }
    // Copy texcoords
    for (i = 0; i < dst.ntexc; i++) {
        memcpy(dst.texcoords[i], src.texcoords[i], 3 * sizeof(float));
    }
    // Copy normals
    for (i = 0; i < dst.nnorm; i++) {
        memcpy(dst.normals[i], src.normals[i], 3 * sizeof(float));
    }
    // Copy parameters
    for (i = 0; i < dst.nparam; i++) {
        memcpy(dst.params[i], src.params[i], 3 * sizeof(float));
    }
    // Copy points
    if (dst.npoint > 0) {
        memcpy(dst.nvert_in_point, src.nvert_in_point, dst.npoint * sizeof(int));
        for (i = 0; i < dst.npoint; i++) {
            int *ipoint = (int *) realloc(dst.points[i], src.nvert_in_point[i] * sizeof(int));
            if (ipoint == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate point %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.points[i] = ipoint;
            memcpy(dst.points[i], src.points[i], src.nvert_in_point[i] * sizeof(int));
        }
    }
    // Copy lines
    if (dst.nline > 0) {
        memcpy(dst.nvert_in_line, src.nvert_in_line, dst.nline * sizeof(int));
        for (i = 0; i < dst.nline; i++) {
            int *iline = (int *) realloc(dst.lines[i], src.nvert_in_line[i] * sizeof(int));
            if (iline == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate line %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.lines[i] = iline;
            memcpy(dst.lines[i], src.lines[i], src.nvert_in_line[i] * sizeof(int));
        }
        if (src.line_texcoords == nullptr) {
            free(dst.line_texcoords);
            dst.line_texcoords = nullptr;
        } else {
            for (i = 0; i < dst.nline; i++) {
                int *iline_texcoord = (int *) realloc(dst.line_texcoords[i], src.nvert_in_line[i] * sizeof(int));
                if (iline_texcoord == nullptr) {

                    communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate line texcoord %d.", i);
                    free_obj(&dst);
                    return dst;
                }
                dst.line_texcoords[i] = iline_texcoord;
                memcpy(dst.line_texcoords[i], src.line_texcoords[i], src.nvert_in_line[i] * sizeof(int));
            }
        }
    }
    // Copy faces
    if (dst.nface > 0) {
        memcpy(dst.nvert_in_face, src.nvert_in_face, dst.nface * sizeof(int));
        for (i = 0; i < dst.nface; i++) {
            int *iface = (int *) realloc(dst.faces[i], src.nvert_in_face[i] * sizeof(int));
            if (iface == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate face %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.faces[i] = iface;
            memcpy(dst.faces[i], src.faces[i], src.nvert_in_face[i] * sizeof(int));
        }
        if (src.face_texcoords == nullptr) {
            free(dst.face_texcoords);
            dst.face_texcoords = nullptr;
        } else {
            for (i = 0; i < dst.nface; i++) {
                int *iface_texcoord = (int *) realloc(dst.face_texcoords[i], src.nvert_in_face[i] * sizeof(int));
                if (iface_texcoord == nullptr) {

                    communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate face texcoord %d.", i);
                    free_obj(&dst);
                    return dst;
                }
                dst.face_texcoords[i] = iface_texcoord;
                memcpy(dst.face_texcoords[i], src.face_texcoords[i], src.nvert_in_face[i] * sizeof(int));
            }
        }
        if (src.face_normals == nullptr) {
            free(dst.face_normals);
            dst.face_normals = nullptr;
        } else {
            for (i = 0; i < dst.nface; i++) {
                int *iface_texcoord = (int *) realloc(dst.face_normals[i], src.nvert_in_face[i] * sizeof(int));
                if (iface_texcoord == nullptr) {

                    communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate face texcoord %d.", i);
                    free_obj(&dst);
                    return dst;
                }
                dst.face_normals[i] = iface_texcoord;
                memcpy(dst.face_normals[i], src.face_normals[i], src.nvert_in_face[i] * sizeof(int));
            }
        }
    }
    // Copy curves
    if (dst.ncurve > 0) {
        memcpy(dst.nvert_in_curve, src.nvert_in_curve, dst.ncurve * sizeof(int));
        for (i = 0; i < dst.ncurve; i++) {
            int *icurve = (int *) realloc(dst.curves[i], src.nvert_in_curve[i] * sizeof(int));
            if (icurve == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate curve %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.curves[i] = icurve;
            memcpy(dst.curves[i], src.curves[i], src.nvert_in_curve[i] * sizeof(int));
        }
        for (i = 0; i < dst.ncurve; i++) {
            memcpy(dst.curve_params[i], src.curve_params[i], 2 * sizeof(float));
        }
    }
    // Copy curves2
    if (dst.ncurve2 > 0) {
        memcpy(dst.nparam_in_curve2, src.nparam_in_curve2, dst.ncurve2 * sizeof(int));
        for (i = 0; i < dst.ncurve2; i++) {
            int *icurve2 = (int *) realloc(dst.curves2[i], src.nparam_in_curve2[i] * sizeof(int));
            if (icurve2 == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate curve2 %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.curves2[i] = icurve2;
            memcpy(dst.curves2[i], src.curves2[i], src.nparam_in_curve2[i] * sizeof(int));
        }
    }
    // Copy surfaces
    if (dst.nsurf > 0) {
        memcpy(dst.nvert_in_surface, src.nvert_in_surface, dst.nsurf * sizeof(int));
        for (i = 0; i < dst.nsurf; i++) {
            int *isurface = (int *) realloc(dst.surfaces[i], src.nvert_in_surface[i] * sizeof(int));
            if (isurface == nullptr) {
                communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate surface %d.", i);
                free_obj(&dst);
                return dst;
            }
            dst.surfaces[i] = isurface;
            memcpy(dst.surfaces[i], src.surfaces[i], src.nvert_in_surface[i] * sizeof(int));
        }
        for (i = 0; i < dst.nsurf; i++) {
            memcpy(dst.surface_params_u[i], src.surface_params_u[i], 2 * sizeof(float));
        }
        for (i = 0; i < dst.nsurf; i++) {
            memcpy(dst.surface_params_v[i], src.surface_params_v[i], 2 * sizeof(float));
        }
        if (src.surface_texcoords == nullptr) {
            free(dst.surface_texcoords);
            dst.surface_texcoords = nullptr;
        } else {
            for (i = 0; i < dst.nsurf; i++) {
                int *isurface_texcoord = (int *) realloc(dst.surface_texcoords[i],
                                                         src.nvert_in_surface[i] * sizeof(int));
                if (isurface_texcoord == nullptr) {

                    communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate surface texcoord %d.", i);
                    free_obj(&dst);
                    return dst;
                }
                dst.surface_texcoords[i] = isurface_texcoord;
                memcpy(dst.surface_texcoords[i], src.surface_texcoords[i], src.nvert_in_surface[i] * sizeof(int));
            }
        }
        if (src.surface_normals == nullptr) {
            free(dst.surface_normals);
            dst.surface_normals = nullptr;
        } else {
            for (i = 0; i < dst.nsurf; i++) {
                int *isurface_texcoord = (int *) realloc(dst.surface_normals[i], src.nvert_in_surface[i] * sizeof(int));
                if (isurface_texcoord == nullptr) {

                    communication::utils::ygglog_error("ObjDict::copy_obj: Could not allocate surface texcoord %d.", i);
                    free_obj(&dst);
                    return dst;
                }
                dst.surface_normals[i] = isurface_texcoord;
                memcpy(dst.surface_normals[i], src.surface_normals[i], src.nvert_in_surface[i] * sizeof(int));
            }
        }
    }
    return dst;
}

void display_obj_indent(obj_t p, const char* indent) {
    int i, j;
    printf("%sMaterial: %s\n", indent, p.material);
    printf("%s%d Vertices:\n", indent, p.nvert);
    for (i = 0; i < p.nvert; i++) {
        printf("%s  %f, %f, %f, %f\n", indent,
               p.vertices[i][0], p.vertices[i][1], p.vertices[i][2], p.vertices[i][3]);
    }
    printf("%s%d Texcoords:\n", indent, p.ntexc);
    for (i = 0; i < p.ntexc; i++) {
        printf("%s  %f, %f, %f\n", indent,
               p.texcoords[i][0], p.texcoords[i][1], p.texcoords[i][2]);
    }
    printf("%s%d Normals:\n", indent, p.nnorm);
    for (i = 0; i < p.nnorm; i++) {
        printf("%s  %f, %f, %f\n", indent,
               p.normals[i][0], p.normals[i][1], p.normals[i][2]);
    }
    printf("%s%d Params:\n", indent, p.nparam);
    for (i = 0; i < p.nparam; i++) {
        printf("%s  %f, %f, %f\n", indent,
               p.params[i][0], p.params[i][1], p.params[i][2]);
    }
    printf("%s%d Points:\n", indent, p.npoint);
    for (i = 0; i < p.npoint; i++) {
        printf("%s  %d", indent, p.points[i][0]);
        for (j = 1; j < p.nvert_in_point[i]; j++)
            printf(", %d", p.points[i][j]);
        printf("\n");
    }
    printf("%s%d Lines:\n", indent, p.nline);
    for (i = 0; i < p.nline; i++) {
        printf("%s  %d", indent, p.lines[i][0]);
        for (j = 1; j < p.nvert_in_line[i]; j++)
            printf(", %d", p.lines[i][j]);
        printf("\n");
    }
    printf("%s%d Faces:\n", indent, p.nface);
    for (i = 0; i < p.nface; i++) {
        printf("%s  %d", indent, p.faces[i][0]);
        for (j = 1; j < p.nvert_in_face[i]; j++)
            printf(", %d", p.faces[i][j]);
        printf("\n");
    }
    printf("%s%d Curves:\n", indent, p.ncurve);
    for (i = 0; i < p.ncurve; i++) {
        printf("%s  %f  %f  %d", indent,
               p.curve_params[i][0], p.curve_params[i][1],
               p.curves[i][0]);
        for (j = 1; j < p.nvert_in_curve[i]; j++)
            printf(", %d", p.curves[i][j]);
        printf("\n");
    }
    printf("%s%d Curve2s:\n", indent, p.ncurve2);
    for (i = 0; i < p.ncurve2; i++) {
        printf("%s  %d", indent, p.curves2[i][0]);
        for (j = 1; j < p.nparam_in_curve2[i]; j++)
            printf(", %d", p.curves2[i][j]);
        printf("\n");
    }
    printf("%s%d Surfaces:\n", indent, p.nsurf);
    for (i = 0; i < p.nsurf; i++) {
        printf("%s  %f  %f  %f  %f  %d", indent,
               p.surface_params_u[i][0], p.surface_params_u[i][1],
               p.surface_params_v[i][0], p.surface_params_v[i][1],
               p.surfaces[i][0]);
        for (j = 1; j < p.nvert_in_surface[i]; j++)
            printf(", %d", p.surfaces[i][j]);
        printf("\n");
    }
}

void display_obj(obj_t p) {
    display_obj_indent(p, "");
}
