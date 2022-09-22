#pragma once
namespace communication {
namespace datatypes {
namespace Metaschema {

class Dict {
public:
    virtual void display_indent(const char* indent) const = 0;
    virtual void display() const = 0;

    char material[100]; //!< Name of material.
    int nvert; //!< Number of vertices.
    int nface; //!< Number of faces.
    float **vertices; //!< X, Y, Z positions of vertices.
    int **faces; //!< Indices of the vertices composing each face.
    int **vertex_colors; //!< RGB colors of each verte
    int *nvert_in_face; //!< Number of vertices in each face.

protected:
    Dict() {
        material[0] = '\0';
        nvert = 0;
        nface = 0;
        vertices = nullptr;
        faces = nullptr;
        vertex_colors = nullptr;
        nvert_in_face = nullptr;

    }
    ~Dict();

};

}
}
}
