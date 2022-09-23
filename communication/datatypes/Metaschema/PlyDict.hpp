#pragma once
#include "Dict.hpp"
namespace communication {
namespace datatypes {
namespace Metaschema {


class PlyDict : public Dict {
public:
    PlyDict();
    PlyDict(const PlyDict* src);
    ~PlyDict();
    void display_indent(const char* indent) const override;
    void display() const override;
    int alloc(int nvert, int nface, int nedge, int do_vert_color, int do_edge_color);

private:
    int nedge; //!< Number of edges.
    int **edges; //!< Indices of the vertices composing each edge.
    int **edge_colors; //!< RGB colors of each edge.
};
}
}
}
