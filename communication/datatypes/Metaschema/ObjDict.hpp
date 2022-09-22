#pragma once

#include "utils/tools.hpp"
#include "Dict.hpp"
namespace communication {
namespace datatypes {
namespace Metaschema {


class ObjDict : public Dict {
public:
    ObjDict();

    ObjDict(const ObjDict* obj);
    ~ObjDict();

    int alloc(int nvert, int ntexc, int nnorm, int nparam,
              int npoint, int nline, int nface, int ncurve, int ncurve2,
              int nsurf, int do_color);

    void display_indent(const char* indent) const override;
    void display() const override { display_indent("");}

    int ntexc; //!< Number of texture coordinates.
    int nnorm; //!< Number of normals.
    int nparam; //!< Number of params.
    int npoint; //!< Number of points.
    int nline; //!< Number of lines.
    int ncurve; //!< Number of curves.
    int ncurve2; //!< Number of curv2.
    int nsurf; //!< Number of surfaces.
    float **texcoords; //!< Texture coordinates.
    float **normals; //!< X, Y, Z direction of normals.
    float **params; //!< U, V, W directions of params.
    int **points; //!< Sets of one or more vertex indices.
    int *nvert_in_point; //!< Number of vertex indices in each point set.
    int **lines; //!< Indices of the vertices composing each line.
    int *nvert_in_line; //!< Number of vertex indices in each line.
    int **line_texcoords; //!< Indices of texcoords for each line vertex.
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

};
}
}
}