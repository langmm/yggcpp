#include "ObjDict.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

ObjDict::ObjDict() : Dict() {
    ntexc = 0;
    nnorm = 0;
    nparam = 0;
    npoint = 0;
    nline = 0;
    ncurve = 0;
    ncurve2 = 0;
    nsurf = 0;
    texcoords = nullptr;
    normals = nullptr;
    params = nullptr;
    points = nullptr;
    nvert_in_point = nullptr;
    lines = nullptr;
    nvert_in_line = nullptr;
    line_texcoords = nullptr;
    face_texcoords = nullptr;
    face_normals = nullptr;
    curves = nullptr;
    curve_params = nullptr;
    nvert_in_curve = nullptr;
    curves2 = nullptr;
    nparam_in_curve2 = nullptr;
    surfaces = nullptr;
    nvert_in_surface = nullptr;
    surface_params_u = nullptr;
    surface_params_v = nullptr;
    surface_texcoords = nullptr;
    surface_normals = nullptr;
}

ObjDict::~ObjDict() {
    int i;
    // Vertices
    if (vertices != nullptr) {
        for (i = 0; i < nvert; i++) {
            if (vertices[i] != nullptr) {
                free(vertices[i]);
                vertices[i] = nullptr;
            }
        }
        free(vertices);
        vertices = nullptr;
    }
    if (vertex_colors != nullptr) {
        for (i = 0; i < nvert; i++) {
            if (vertex_colors[i] != nullptr) {
                free(vertex_colors[i]);
                vertex_colors[i] = nullptr;
            }
        }
        free(vertex_colors);
        vertex_colors = nullptr;
    }
    // Texcoords
    if (texcoords != nullptr) {
        for (i = 0; i < ntexc; i++) {
            if (texcoords[i] != nullptr) {
                free(texcoords[i]);
                texcoords[i] = nullptr;
            }
        }
        free(texcoords);
        texcoords = nullptr;
    }
    // Normals
    if (normals != nullptr) {
        for (i = 0; i < nnorm; i++) {
            if (normals[i] != nullptr) {
                free(normals[i]);
                normals[i] = nullptr;
            }
        }
        free(normals);
        normals = nullptr;
    }
    // Parameters
    if (params != nullptr) {
        for (i = 0; i < nparam; i++) {
            if (params[i] != nullptr) {
                free(params[i]);
                params[i] = nullptr;
            }
        }
        free(params);
        params = nullptr;
    }
    // Points
    if (points != nullptr) {
        for (i = 0; i < npoint; i++) {
            if (points[i] != nullptr) {
                free(points[i]);
                points[i] = nullptr;
            }
        }
        free(points);
        points = nullptr;
    }
    if (nvert_in_point != nullptr) {
        free(nvert_in_point);
        nvert_in_point = nullptr;
    }
    // Lines
    if (lines != nullptr) {
        for (i = 0; i < nline; i++) {
            if (lines[i] != nullptr) {
                free(lines[i]);
                lines[i] = nullptr;
            }
        }
        free(lines);
        lines = nullptr;
    }
    if (nvert_in_line != nullptr) {
        free(nvert_in_line);
        nvert_in_line = nullptr;
    }
    if (line_texcoords != nullptr) {
        for (i = 0; i < nline; i++) {
            if (line_texcoords[i] != nullptr) {
                free(line_texcoords[i]);
                line_texcoords[i] = nullptr;
            }
        }
        free(line_texcoords);
        line_texcoords = nullptr;
    }
    // Faces
    if (faces != nullptr) {
        for (i = 0; i < nface; i++) {
            if (faces[i] != nullptr) {
                free(faces[i]);
                faces[i] = nullptr;
            }
        }
        free(faces);
        faces = nullptr;
    }
    if (nvert_in_face != nullptr) {
        free(nvert_in_face);
        nvert_in_face = nullptr;
    }
    if (face_texcoords != nullptr) {
        for (i = 0; i < nface; i++) {
            if (face_texcoords[i] != nullptr) {
                free(face_texcoords[i]);
                face_texcoords[i] = nullptr;
            }
        }
        free(face_texcoords);
        face_texcoords = nullptr;
    }
    if (face_normals != nullptr) {
        for (i = 0; i < nface; i++) {
            if (face_normals[i] != nullptr) {
                free(face_normals[i]);
                face_normals[i] = nullptr;
            }
        }
        free(face_normals);
        face_normals = nullptr;
    }
    // Curves
    if (curves != nullptr) {
        for (i = 0; i < ncurve; i++) {
            if (curves[i] != nullptr) {
                free(curves[i]);
                curves[i] = nullptr;
            }
        }
        free(curves);
        curves = nullptr;
    }
    if (curve_params != nullptr) {
        for (i = 0; i < ncurve; i++) {
            if (curve_params[i] != nullptr) {
                free(curve_params[i]);
                curve_params[i] = nullptr;
            }
        }
        free(curve_params);
        curve_params = nullptr;
    }
    if (nvert_in_curve != nullptr) {
        free(nvert_in_curve);
        nvert_in_curve = nullptr;
    }
    // Curves2
    if (curves2 != nullptr) {
        for (i = 0; i < ncurve2; i++) {
            if (curves2[i] != nullptr) {
                free(curves2[i]);
                curves2[i] = nullptr;
            }
        }
        free(curves2);
        curves2 = nullptr;
    }
    if (nparam_in_curve2 != nullptr) {
        free(nparam_in_curve2);
        nparam_in_curve2 = nullptr;
    }
    // Surfaces
    if (surfaces != nullptr) {
        for (i = 0; i < nsurf; i++) {
            if (surfaces[i] != nullptr) {
                free(surfaces[i]);
                surfaces[i] = nullptr;
            }
        }
        free(surfaces);
        surfaces = nullptr;
    }
    if (nvert_in_surface != nullptr) {
        free(nvert_in_surface);
        nvert_in_surface = nullptr;
    }
    if (surface_params_u != nullptr) {
        for (i = 0; i < nsurf; i++) {
            if (surface_params_u[i] != nullptr) {
                free(surface_params_u[i]);
                surface_params_u[i] = nullptr;
            }
        }
        free(surface_params_u);
        surface_params_u = nullptr;
    }
    if (surface_params_v != nullptr) {
        for (i = 0; i < nsurf; i++) {
            if (surface_params_v[i] != nullptr) {
                free(surface_params_v[i]);
                surface_params_v[i] = nullptr;
            }
        }
        free(surface_params_v);
        surface_params_v = nullptr;
    }
    if (surface_texcoords != nullptr) {
        for (i = 0; i < nsurf; i++) {
            if (surface_texcoords[i] != nullptr) {
                free(surface_texcoords[i]);
                surface_texcoords[i] = nullptr;
            }
        }
        free(surface_texcoords);
        surface_texcoords = nullptr;
    }
    if (surface_normals != nullptr) {
        for (i = 0; i < nsurf; i++) {
            if (surface_normals[i] != nullptr) {
                free(surface_normals[i]);
                surface_normals[i] = nullptr;
            }
        }
        free(surface_normals);
        surface_normals = nullptr;
    }
    // Counts
    material[0] = '\0';
    nvert = 0;
    ntexc = 0;
    nnorm = 0;
    nparam = 0;
    npoint = 0;
    nline = 0;
    nface = 0;
    ncurve = 0;
    ncurve2 = 0;
    nsurf = 0;
}

int ObjDict::alloc(int nvert, int ntexc, int nnorm, int nparam,
              int npoint, int nline, int nface, int ncurve, int ncurve2,
              int nsurf, int do_color) {
    int i;
    // TODO  free_obj(p); // Ensure that existing data is freed
    this->nvert = nvert;
    this->ntexc = ntexc;
    this->nnorm = nnorm;
    this->nparam = nparam;
    this->npoint = npoint;
    this->nline = nline;
    this->nface = nface;
    this->ncurve = ncurve;
    this->ncurve2 = ncurve2;
    this->nsurf = nsurf;
    // Allocate vertices
    if (nvert > 0) {
        float **new_vert = (float **) malloc(nvert * sizeof(float *));
        if (new_vert == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate vertices.");
            free_obj(p);
            return -1;
        }
        vertices = new_vert;
        for (i = 0; i < nvert; i++) {
            float *ivert = (float *) malloc(4 * sizeof(float));
            if (ivert == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate vertex %d.", i);
                free_obj(p);
                return -1;
            }
            vertices[i] = ivert;
        }
        ygglog_debug("alloc_obj: Allocated %d vertices.", nvert);
    }
    // Allocate vertex colors
    if ((nvert > 0) && (do_color)) {
        int **new_vert_colors = (int **) malloc(nvert * sizeof(int *));
        if (new_vert_colors == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate vertex_colors.");
            free_obj(p);
            return -1;
        }
        vertex_colors = new_vert_colors;
        for (i = 0; i < nvert; i++) {
            int *ivert = (int *) malloc(3 * sizeof(int));
            if (ivert == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate vertex color %d.", i);
                free_obj(p);
                return -1;
            }
            vertex_colors[i] = ivert;
        }
        ygglog_debug("alloc_obj: Allocated %d vertex colors.", nvert);
    }
    // Allocate texcoords
    if (ntexc > 0) {
        float **new_texc = (float **) malloc(ntexc * sizeof(float *));
        if (new_texc == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate texcoords.");
            free_obj(p);
            return -1;
        }
        texcoords = new_texc;
        for (i = 0; i < ntexc; i++) {
            float *itexc = (float *) malloc(3 * sizeof(float));
            if (itexc == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate texcoord %d.", i);
                free_obj(p);
                return -1;
            }
            texcoords[i] = itexc;
        }
        ygglog_debug("alloc_obj: Allocated %d texcoords.", ntexc);
    }
    // Allocate normals
    if (nnorm > 0) {
        float **new_norm = (float **) malloc(nnorm * sizeof(float *));
        if (new_norm == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate normals.");
            free_obj(p);
            return -1;
        }
        normals = new_norm;
        for (i = 0; i < nnorm; i++) {
            float *inorm = (float *) malloc(3 * sizeof(float));
            if (inorm == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate normal %d.", i);
                free_obj(p);
                return -1;
            }
            normals[i] = inorm;
        }
        ygglog_debug("alloc_obj: Allocated %d normals.", nnorm);
    }
    // Allocate parameters
    if (nparam > 0) {
        float **new_param = (float **) malloc(nparam * sizeof(float *));
        if (new_param == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate params.");
            free_obj(p);
            return -1;
        }
        params = new_param;
        for (i = 0; i < nparam; i++) {
            float *iparam = (float *) malloc(3 * sizeof(float));
            if (iparam == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate param %d.", i);
                free_obj(p);
                return -1;
            }
            params[i] = iparam;
        }
        ygglog_debug("alloc_obj: Allocated %d params.", nparam);
    }
    // Allocate points
    if (npoint > 0) {
        int **new_point = (int **) malloc(npoint * sizeof(int *));
        if (new_point == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate points.");
            free_obj(p);
            return -1;
        }
        points = new_point;
        for (i = 0; i < npoint; i++) {
            points[i] = nullptr;
        }
        int *new_nvert_point = (int *) malloc(npoint * sizeof(int));
        if (new_nvert_point == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nvert_in_point.");
            free_obj(p);
            return -1;
        }
        nvert_in_point = new_nvert_point;
        for (i = 0; i < npoint; i++) {
            nvert_in_point[i] = 0;
        }
    }
    // Allocate lines
    if (nline > 0) {
        int **new_line = (int **) malloc(nline * sizeof(int *));
        if (new_line == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate lines.");
            free_obj(p);
            return -1;
        }
        lines = new_line;
        for (i = 0; i < nline; i++) {
            lines[i] = nullptr;
        }
        int *new_nvert_line = (int *) malloc(nline * sizeof(int));
        if (new_nvert_line == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nvert_in_line.");
            free_obj(p);
            return -1;
        }
        nvert_in_line = new_nvert_line;
        for (i = 0; i < nline; i++) {
            nvert_in_line[i] = 0;
        }
        int **new_line_texcoords = (int **) malloc(nline * sizeof(int *));
        if (new_line_texcoords == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate line_texcoords.");
            free_obj(p);
            return -1;
        }
        line_texcoords = new_line_texcoords;
        for (i = 0; i < nline; i++) {
            line_texcoords[i] = nullptr;
        }
    }
    // Allocate faces
    if (nface > 0) {
        int **new_face = (int **) malloc(nface * sizeof(int *));
        if (new_face == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate faces.");
            free_obj(p);
            return -1;
        }
        faces = new_face;
        for (i = 0; i < nface; i++) {
            faces[i] = nullptr;
        }
        int *new_nvert_face = (int *) malloc(nface * sizeof(int));
        if (new_nvert_face == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nvert_in_face.");
            free_obj(p);
            return -1;
        }
        nvert_in_face = new_nvert_face;
        for (i = 0; i < nface; i++) {
            nvert_in_face[i] = 0;
        }
        int **new_face_texcoords = (int **) malloc(nface * sizeof(int *));
        if (new_face_texcoords == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate face_texcoords.");
            free_obj(p);
            return -1;
        }
        face_texcoords = new_face_texcoords;
        for (i = 0; i < nface; i++) {
            face_texcoords[i] = nullptr;
        }
        int **new_face_normals = (int **) malloc(nface * sizeof(int *));
        if (new_face_normals == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate face_normals.");
            free_obj(p);
            return -1;
        }
        face_normals = new_face_normals;
        for (i = 0; i < nface; i++) {
            face_normals[i] = nullptr;
        }
        ygglog_debug("alloc_obj: Allocated %d faces.", nface);
    }
    // Allocate curves
    if (ncurve > 0) {
        int **new_curve = (int **) malloc(ncurve * sizeof(int *));
        if (new_curve == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate curves.");
            free_obj(p);
            return -1;
        }
        curves = new_curve;
        for (i = 0; i < ncurve; i++) {
            curves[i] = nullptr;
        }
        int *new_nvert_curve = (int *) malloc(ncurve * sizeof(int));
        if (new_nvert_curve == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nvert_in_curve.");
            free_obj(p);
            return -1;
        }
        nvert_in_curve = new_nvert_curve;
        for (i = 0; i < ncurve; i++) {
            nvert_in_curve[i] = 0;
        }
        float **new_curve_params = (float **) malloc(ncurve * sizeof(float *));
        if (new_curve_params == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate curve_params.");
            free_obj(p);
            return -1;
        }
        curve_params = new_curve_params;
        for (i = 0; i < ncurve; i++) {
            float *iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate curve param %d.", i);
                free_obj(p);
                return -1;
            }
            curve_params[i] = iparam;
        }
    }
    // Curves2
    if (ncurve2 > 0) {
        int **new_curve2 = (int **) malloc(ncurve2 * sizeof(int *));
        if (new_curve2 == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate curves2.");
            free_obj(p);
            return -1;
        }
        curves2 = new_curve2;
        for (i = 0; i < ncurve2; i++) {
            curves2[i] = nullptr;
        }
        int *new_nparam_curve2 = (int *) malloc(ncurve2 * sizeof(int));
        if (new_nparam_curve2 == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nparam_in_curve2.");
            free_obj(p);
            return -1;
        }
        nparam_in_curve2 = new_nparam_curve2;
        for (i = 0; i < ncurve2; i++) {
            nparam_in_curve2[i] = 0;
        }
    }
    // Surfaces
    if (nsurf > 0) {
        int **new_surface = (int **) malloc(nsurf * sizeof(int *));
        if (new_surface == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate surfaces.");
            free_obj(p);
            return -1;
        }
        surfaces = new_surface;
        for (i = 0; i < nsurf; i++) {
            surfaces[i] = nullptr;
        }
        int *new_nvert_surface = (int *) malloc(nsurf * sizeof(int));
        if (new_nvert_surface == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate nvert_in_surface.");
            free_obj(p);
            return -1;
        }
        nvert_in_surface = new_nvert_surface;
        for (i = 0; i < nsurf; i++) {
            nvert_in_surface[i] = 0;
        }
        float **new_surface_params_u = (float **) malloc(nsurf * sizeof(float *));
        if (new_surface_params_u == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate surface_params_u.");
            free_obj(p);
            return -1;
        }
        surface_params_u = new_surface_params_u;
        for (i = 0; i < nsurf; i++) {
            float *iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate surface param %d.", i);
                free_obj(p);
                return -1;
            }
            surface_params_u[i] = iparam;
        }
        float **new_surface_params_v = (float **) malloc(nsurf * sizeof(float *));
        if (new_surface_params_v == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate surface_params_v.");
            free_obj(p);
            return -1;
        }
        surface_params_v = new_surface_params_v;
        for (i = 0; i < nsurf; i++) {
            float *iparam = (float *) malloc(2 * sizeof(float));
            if (iparam == nullptr) {
                ygglog_error("alloc_obj: Failed to allocate surface param %d.", i);
                free_obj(p);
                return -1;
            }
            surface_params_v[i] = iparam;
        }
        int **new_surface_texcoords = (int **) malloc(nsurf * sizeof(int *));
        if (new_surface_texcoords == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate surface_texcoords.");
            free_obj(p);
            return -1;
        }
        surface_texcoords = new_surface_texcoords;
        for (i = 0; i < nsurf; i++) {
            surface_texcoords[i] = nullptr;
        }
        int **new_surface_normals = (int **) malloc(nsurf * sizeof(int *));
        if (new_surface_normals == nullptr) {
            ygglog_error("alloc_obj: Failed to allocate surface_normals.");
            free_obj(p);
            return -1;
        }
        surface_normals = new_surface_normals;
        for (i = 0; i < nsurf; i++) {
            surface_normals[i] = nullptr;
        }
    }
    // Return
    ygglog_debug("alloc_obj: Allocated for\n"
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
                 nvert, ntexc, nnorm, nparam, npoint,
                 nline, nface, ncurve, ncurve2, nsurf);
    return 0;
}

ObjDict::ObjDict(const ObjDict *src) : ObjDict() {
    int i;
    int do_color = 0;
    if (src->vertex_colors != NULL) {
        do_color = 1;
    }
    alloc_obj(src->nvert, src->ntexc, src->nnorm, src->nparam,
              src->npoint, src->nline, src->nface, src->ncurve, src->ncurve2,
              src->nsurf, do_color);
    strcpy(this->material, src->material);
    // Copy vertices
    for (i = 0; i < this->nvert; i++) {
        memcpy(this->vertices[i], src->vertices[i], 4*sizeof(float));
    }
    // Copy vertex colors
    if (do_color) {
        for (i = 0; i < this->nvert; i++) {
            memcpy(this->vertex_colors[i], src->vertex_colors[i], 3*sizeof(int));
        }
    }
    // Copy texcoords
    for (i = 0; i < this->ntexc; i++) {
        memcpy(this->texcoords[i], src->texcoords[i], 3*sizeof(float));
    }
    // Copy normals
    for (i = 0; i < this->nnorm; i++) {
        memcpy(this->normals[i], src->normals[i], 3*sizeof(float));
    }
    // Copy parameters
    for (i = 0; i < this->nparam; i++) {
        memcpy(this->params[i], src->params[i], 3*sizeof(float));
    }
    // Copy points
    if (this->npoint > 0) {
        memcpy(this->nvert_in_point, src->nvert_in_point, this->npoint*sizeof(int));
        for (i = 0; i < this->npoint; i++) {
            int *ipoint = (int*)realloc(this->points[i], src->nvert_in_point[i]*sizeof(int));
            if (ipoint == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate point %d.", i);
                free_obj(&dst);
                return;
            }
            this->points[i] = ipoint;
            memcpy(this->points[i], src->points[i], src->nvert_in_point[i]*sizeof(int));
        }
    }
    // Copy lines
    if (this->nline > 0) {
        memcpy(this->nvert_in_line, src->nvert_in_line, this->nline*sizeof(int));
        for (i = 0; i < this->nline; i++) {
            int *iline = (int*)realloc(this->lines[i], src->nvert_in_line[i]*sizeof(int));
            if (iline == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate line %d.", i);
                free_obj(&dst);
                return;
            }
            this->lines[i] = iline;
            memcpy(this->lines[i], src->lines[i], src->nvert_in_line[i]*sizeof(int));
        }
        if (src->line_texcoords == NULL) {
            free(this->line_texcoords);
            this->line_texcoords = NULL;
        } else {
            for (i = 0; i < this->nline; i++) {
                int *iline_texcoord = (int*)realloc(this->line_texcoords[i], src->nvert_in_line[i]*sizeof(int));
                if (iline_texcoord == NULL) {

                    ygglog_error("ObjDict::copy_obj: Could not allocate line texcoord %d.", i);
                    free_obj(&dst);
                    return;
                }
                this->line_texcoords[i] = iline_texcoord;
                memcpy(this->line_texcoords[i], src->line_texcoords[i], src->nvert_in_line[i]*sizeof(int));
            }
        }
    }
    // Copy faces
    if (this->nface > 0) {
        memcpy(this->nvert_in_face, src->nvert_in_face, this->nface*sizeof(int));
        for (i = 0; i < this->nface; i++) {
            int *iface = (int*)realloc(this->faces[i], src->nvert_in_face[i]*sizeof(int));
            if (iface == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate face %d.", i);
                free_obj(&dst);
                return;
            }
            this->faces[i] = iface;
            memcpy(this->faces[i], src->faces[i], src->nvert_in_face[i]*sizeof(int));
        }
        if (src->face_texcoords == NULL) {
            free(this->face_texcoords);
            this->face_texcoords = NULL;
        } else {
            for (i = 0; i < this->nface; i++) {
                int *iface_texcoord = (int*)realloc(this->face_texcoords[i], src->nvert_in_face[i]*sizeof(int));
                if (iface_texcoord == NULL) {

                    ygglog_error("ObjDict::copy_obj: Could not allocate face texcoord %d.", i);
                    free_obj(&dst);
                    return;
                }
                this->face_texcoords[i] = iface_texcoord;
                memcpy(this->face_texcoords[i], src->face_texcoords[i], src->nvert_in_face[i]*sizeof(int));
            }
        }
        if (src->face_normals == NULL) {
            free(this->face_normals);
            this->face_normals = NULL;
        } else {
            for (i = 0; i < this->nface; i++) {
                int *iface_texcoord = (int*)realloc(this->face_normals[i], src->nvert_in_face[i]*sizeof(int));
                if (iface_texcoord == NULL) {

                    ygglog_error("ObjDict::copy_obj: Could not allocate face texcoord %d.", i);
                    free_obj(&dst);
                    return;
                }
                this->face_normals[i] = iface_texcoord;
                memcpy(this->face_normals[i], src->face_normals[i], src->nvert_in_face[i]*sizeof(int));
            }
        }
    }
    // Copy curves
    if (this->ncurve > 0) {
        memcpy(this->nvert_in_curve, src->nvert_in_curve, this->ncurve*sizeof(int));
        for (i = 0; i < this->ncurve; i++) {
            int *icurve = (int*)realloc(this->curves[i], src->nvert_in_curve[i]*sizeof(int));
            if (icurve == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate curve %d.", i);
                free_obj(&dst);
                return;
            }
            this->curves[i] = icurve;
            memcpy(this->curves[i], src->curves[i], src->nvert_in_curve[i]*sizeof(int));
        }
        for (i = 0; i < this->ncurve; i++) {
            memcpy(this->curve_params[i], src->curve_params[i], 2*sizeof(float));
        }
    }
    // Copy curves2
    if (this->ncurve2 > 0) {
        memcpy(this->nparam_in_curve2, src->nparam_in_curve2, this->ncurve2*sizeof(int));
        for (i = 0; i < this->ncurve2; i++) {
            int *icurve2 = (int*)realloc(this->curves2[i], src->nparam_in_curve2[i]*sizeof(int));
            if (icurve2 == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate curve2 %d.", i);
                free_obj(&dst);
                return;
            }
            this->curves2[i] = icurve2;
            memcpy(this->curves2[i], src->curves2[i], src->nparam_in_curve2[i]*sizeof(int));
        }
    }
    // Copy surfaces
    if (this->nsurf > 0) {
        memcpy(this->nvert_in_surface, src->nvert_in_surface, this->nsurf*sizeof(int));
        for (i = 0; i < this->nsurf; i++) {
            int *isurface = (int*)realloc(this->surfaces[i], src->nvert_in_surface[i]*sizeof(int));
            if (isurface == NULL) {
                ygglog_error("ObjDict::copy_obj: Could not allocate surface %d.", i);
                free_obj(&dst);
                return;
            }
            this->surfaces[i] = isurface;
            memcpy(this->surfaces[i], src->surfaces[i], src->nvert_in_surface[i]*sizeof(int));
        }
        for (i = 0; i < this->nsurf; i++) {
            memcpy(this->surface_params_u[i], src->surface_params_u[i], 2*sizeof(float));
        }
        for (i = 0; i < this->nsurf; i++) {
            memcpy(this->surface_params_v[i], src->surface_params_v[i], 2*sizeof(float));
        }
        if (src->surface_texcoords == NULL) {
            free(this->surface_texcoords);
            this->surface_texcoords = NULL;
        } else {
            for (i = 0; i < this->nsurf; i++) {
                int *isurface_texcoord = (int*)realloc(this->surface_texcoords[i], src->nvert_in_surface[i]*sizeof(int));
                if (isurface_texcoord == NULL) {

                    ygglog_error("ObjDict::copy_obj: Could not allocate surface texcoord %d.", i);
                    free_obj(&dst);
                    return;
                }
                this->surface_texcoords[i] = isurface_texcoord;
                memcpy(this->surface_texcoords[i], src->surface_texcoords[i], src->nvert_in_surface[i]*sizeof(int));
            }
        }
        if (src->surface_normals == NULL) {
            free(this->surface_normals);
            this->surface_normals = NULL;
        } else {
            for (i = 0; i < this->nsurf; i++) {
                int *isurface_texcoord = (int*)realloc(this->surface_normals[i], src->nvert_in_surface[i]*sizeof(int));
                if (isurface_texcoord == NULL) {

                    ygglog_error("ObjDict::copy_obj: Could not allocate surface texcoord %d.", i);
                    free_obj(&dst);
                    return;
                }
                this->surface_normals[i] = isurface_texcoord;
                memcpy(this->surface_normals[i], src->surface_normals[i], src->nvert_in_surface[i]*sizeof(int));
            }
        }
    }
}

void ObjDict::display_obj_indent(const char *indent) const {
    int i, j;
    printf("%sMaterial: %s\n", indent, material);
    printf("%s%d Vertices:\n", indent, nvert);
    for (i = 0; i < nvert; i++) {
        printf("%s  %f, %f, %f, %f\n", indent,
               vertices[i][0], vertices[i][1], vertices[i][2], vertices[i][3]);
    }
    printf("%s%d Texcoords:\n", indent, ntexc);
    for (i = 0; i < ntexc; i++) {
        printf("%s  %f, %f, %f\n", indent,
               texcoords[i][0], texcoords[i][1], texcoords[i][2]);
    }
    printf("%s%d Normals:\n", indent, nnorm);
    for (i = 0; i < nnorm; i++) {
        printf("%s  %f, %f, %f\n", indent,
               normals[i][0], normals[i][1], normals[i][2]);
    }
    printf("%s%d Params:\n", indent, nparam);
    for (i = 0; i < nparam; i++) {
        printf("%s  %f, %f, %f\n", indent,
               params[i][0], params[i][1], params[i][2]);
    }
    printf("%s%d Points:\n", indent, npoint);
    for (i = 0; i < npoint; i++) {
        printf("%s  %d", indent, points[i][0]);
        for (j = 1; j < nvert_in_point[i]; j++)
            printf(", %d", points[i][j]);
        printf("\n");
    }
    printf("%s%d Lines:\n", indent, nline);
    for (i = 0; i < nline; i++) {
        printf("%s  %d", indent, lines[i][0]);
        for (j = 1; j < nvert_in_line[i]; j++)
            printf(", %d", lines[i][j]);
        printf("\n");
    }
    printf("%s%d Faces:\n", indent, nface);
    for (i = 0; i < nface; i++) {
        printf("%s  %d", indent, faces[i][0]);
        for (j = 1; j < nvert_in_face[i]; j++)
            printf(", %d", faces[i][j]);
        printf("\n");
    }
    printf("%s%d Curves:\n", indent, ncurve);
    for (i = 0; i < ncurve; i++) {
        printf("%s  %f  %f  %d", indent,
               curve_params[i][0], curve_params[i][1],
               curves[i][0]);
        for (j = 1; j < nvert_in_curve[i]; j++)
            printf(", %d", curves[i][j]);
        printf("\n");
    }
    printf("%s%d Curve2s:\n", indent, ncurve2);
    for (i = 0; i < ncurve2; i++) {
        printf("%s  %d", indent, curves2[i][0]);
        for (j = 1; j < nparam_in_curve2[i]; j++)
            printf(", %d", curves2[i][j]);
        printf("\n");
    }
    printf("%s%d Surfaces:\n", indent, nsurf);
    for (i = 0; i < nsurf; i++) {
        printf("%s  %f  %f  %f  %f  %d", indent,
               surface_params_u[i][0], surface_params_u[i][1],
               surface_params_v[i][0], surface_params_v[i][1],
               surfaces[i][0]);
        for (j = 1; j < nvert_in_surface[i]; j++)
            printf(", %d", surfaces[i][j]);
        printf("\n");
    }
}