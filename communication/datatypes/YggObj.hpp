#pragma once
#include <vector>
#include <rapidjson/obj.h>

namespace communication {
namespace datatypes {

class YggObj : public rapidjson::ObjBase {
public:
    YggObj();
    YggObj(const YggObj* src);

    ~YggObj();

    size_t nvert();
    size_t ntexc();
    size_t nnorm();
    size_t nparam();
    size_t npoint();
    size_t nline();
    size_t nface();
    size_t ncurve();
    size_t ncurve2();
    size_t nsurf();

    std::vector<std::vector<float> > vertices();
    std::vector<std::vector<float> > texcoords();
    std::vector<std::vector<float> > normals();
    std::vector<std::vector<float> > params();
    std::vector<std::vector<float> > curve_params();
    std::vector<std::vector<float> > surface_params_u();
    std::vector<std::vector<float> > surface_params_v();

    std::vector<std::vector<int> > vertex_colors();
    std::vector<std::vector<int> > points();
    std::vector<std::vector<int> > nvert_in_point();
    std::vector<std::vector<int> > lines();
    std::vector<std::vector<int> > nvert_in_line();
    std::vector<std::vector<int> > line_texcoords();
    std::vector<std::vector<int> > faces();
    std::vector<std::vector<int> > nvert_in_face();
    std::vector<std::vector<int> > face_texcoords();
    std::vector<std::vector<int> > face_normals();
    std::vector<std::vector<int> > curves();
    std::vector<std::vector<int> > nvert_in_curve();
    std::vector<std::vector<int> > curves2();
    std::vector<std::vector<int> > nparam_in_curve2();
    std::vector<std::vector<int> > surfaces();
    std::vector<std::vector<int> > surface_texcoords();
    std::vector<std::vector<int> > surface_normals();
    std::vector<std::vector<int> > nvert_in_surface();


private:
    template<typename T>
    std::vector<std::vector<T> > get_array(const std::string& name) {
        std::vector<std::vector<T> > val;
        if (get_property(name, val))
            return val;
        val.clear();
        return val;
    }


};

template<typename T>
void get_data_as_pointers(std::vector<std::vector<T> > &vdat, T** data, size_t &N, size_t &M) {
    N = vdat.size();
    if (N == 0) {
        data = NULL;
        M = 0;
        return;
    }
    M = vdat[0].size();
    data = (T**)malloc(sizeof(T*) * N);
    if (M == 0)
        return;
    for (auto i = 0; i < N; i++) {
        data[i] = (T*)malloc(sizeof(T) * M);
        for (auto j = 0; j < M; j++) {
            data[i][j] = vdat[i][j];
        }
    }
}

}
}
