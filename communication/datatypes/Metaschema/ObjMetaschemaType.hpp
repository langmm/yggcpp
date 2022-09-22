#pragma once

#include "MetaschemaType.hpp"
#include "ObjDict.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {
typedef ObjDict obj_t;
class ObjMetaschemaType : public MetaschemaType {
public:
    ObjMetaschemaType() = delete;

    /*!
      @brief Constructor for ObjMetaschemaType.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ObjMetaschemaType(const bool use_generic = false) : MetaschemaType("obj", use_generic) {}

    /*!
      @brief Constructor for ObjMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ObjMetaschemaType(const rapidjson::Value &type_doc,
                      const bool use_generic = false) : MetaschemaType(type_doc, use_generic) {}

    /*!
      @brief Constructor for ObjMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    ObjMetaschemaType(PyObject *pyobj,
                      const bool use_generic = false) : MetaschemaType(pyobj, use_generic) {}

    /*!
      @brief Copy constructor.
      @param[in] other ObjMetaschemaType* Instance to copy.
     */
    ObjMetaschemaType(const ObjMetaschemaType &other) :
            ObjMetaschemaType(other.use_generic()) {}

    /*!
      @brief Create a copy of the type.
      @returns pointer to new ObjMetaschemaType instance with the same data.
     */
    ObjMetaschemaType *copy() const override { return (new ObjMetaschemaType(use_generic())); }

    /*!
      @brief Copy data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
     */
    Dict* copy_generic(const YggGeneric *data, Dict* orig_data = nullptr) const override;

    /*!
      @brief Free data wrapped in YggGeneric class.
      @param[in] data YggGeneric* Pointer to generic object.
     */
    void free_generic(YggGeneric *data) const override {
        if (data == NULL) {
            utils::ygglog_throw_error("ObjMetaschemaType::free_generic: Generic object is NULL.");
        }
        obj_t **ptr = (obj_t * *)(data->get_data_pointer());
        if (ptr[0] != nullptr) {
            delete ptr[0];
            free(ptr[0]);
            ptr[0] = NULL;
        }
    }

    /*!
      @brief Display data.
      @param[in] data YggGeneric* Pointer to generic object.
      @param[in] indent char* Indentation to add to display output.
     */
    void display_generic(const YggGeneric *data, const char *indent = "") const override {
        if (data == NULL) {
            utils::ygglog_throw_error("ObjMetaschemaType::display_generic: Generic object is NULL.");
        }
        obj_t arg;
        data->get_data(arg);
        arg.display_indent(indent);
    }

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, utils::va_list_t &ap) override;

    /*!
      @brief Get the item size.
      @returns size_t Size of item in bytes.
     */
    const size_t nbytes() const override {
        return sizeof(obj_t);
    }

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    virtual size_t nargs_exp() const override {
        return 1;
    }

    /*!
      @brief Skip arguments that make of this type.
      @param[in, out] nargs Pointer to number of arguments in ap.
      @param[in, out] ap va_list_t Variable argument list.
     */
    void skip_va_elements_core(size_t *nargs, utils::va_list_t *ap) const override {
        va_arg(ap->va, obj_t);
        (*nargs)--;
    }

    /*!
      @brief Convert a Python representation to a C representation.
      @param[in] pyobj PyObject* Pointer to Python object.
      @returns YggGeneric* Pointer to C object.
     */
    YggGeneric *python2c(PyObject *pyobj) const override;

    /*!
      @brief Convert a C representation to a Python representation.
      @param[in] cobj YggGeneric* Pointer to C object.
      @returns PyObject* Pointer to Python object.
     */
    PyObject *c2python(YggGeneric *cobj) const override;

    // Encoding
    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in,out] nargs size_t * Pointer to the number of arguments contained in
      ap. On return it will be set to the number of arguments used.
      @param[in] ap va_list_t Variable number of arguments that should be encoded
      as a JSON string.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     size_t *nargs, utils::va_list_t &ap) const override;

    /*!
      @brief Encode arguments describine an instance of this type into a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @param[in] x YggGeneric* Pointer to generic wrapper for data.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_data(rapidjson::Writer<rapidjson::StringBuffer> *writer,
                     YggGeneric *x) const override {
        size_t nargs = 1;
        obj_t arg;
        x->get_data(arg);
        return MetaschemaType::encode_data(writer, &nargs, &arg);
    }

    // Decoding
    /*!
      @brief Decode a line describine a line entry.
      @param[in,out] p obj_t* Obj struct that should be updated.
      @param[in,out] cline int Reference to count of lines currently in
      p that should be updated when a line is added to the structure.
      @param[in] iline char* Line that should be parsed for line
      information.
      @param[in] re_line_vert const char* Pointer to regex for line
      vertices.
      @param[in] do_texcoords bool If true, texcoords will be extracted
      from the line.
      @returns int -1 if there is an error, 1 otherwise.
    */
    int decode_line(obj_t *p, int &cline, char *iline,
                    const char *re_line_vert,
                    bool do_texcoords) const;

    /*!
      @brief Decode a line describine a face.
      @param[in,out] p obj_t* Obj struct that should be updated.
      @param[in,out] cface int Reference to count of faces currently in
      p that should be updated when a face is added to the structure.
      @param[in] iline char* Line that should be parsed for face
      information.
      @param[in] re_face_vert const char* Pointer to regex for face
      vertices.
      @param[in] do_texcoords bool If true, texcoords will be extracted
      from the line.
      @param[in] do_normals bool If true, normals will be extracted
      from the line.
      @returns int -1 if there is an error, 1 otherwise.
    */
    int decode_face(obj_t *p, int &cface, char *iline,
                    const char *re_face_vert,
                    bool do_texcoords, bool do_normals) const;

    /*!
      @brief Decode a line describine a surface.
      @param[in,out] p obj_t* Obj struct that should be updated.
      @param[in,out] csurf int Reference to count of surfaces currently in
      p that should be updated when a surface is added to the structure.
      @param[in] iline char* Line that should be parsed for surface
      information.
      @param[in] re_surf_vert const char* Pointer to regex for surface
      vertices.
      @param[in] sind_ptr size_t** Pointer to array containing starting
      indices for subexpressions in the regex for the surface entry.
      @param[in] eind_ptr size_t** Pointer to array containing ending
      indices for subexpressions in the regex for the surface entry.
      @param[in] do_texcoords bool If true, texcoords will be extracted
      from the line.
      @param[in] do_normals bool If true, normals will be extracted
      from the line.
      @returns int -1 if there is an error, 1 otherwise.
    */
    int decode_surface(obj_t *p, int &csurf, char *iline,
                       const char *re_surf_vert,
                       size_t **sind_ptr, size_t **eind_ptr,
                       bool do_texcoords, bool do_normals) const;

    /*!
      @brief Decode variables from a JSON string.
      @param[in] data rapidjson::Value Reference to entry in JSON string.
      @param[in] allow_realloc int If 1, the passed variables will be reallocated
      to contain the deserialized data.
      @param[in,out] nargs size_t Number of arguments contained in ap. On return,
      the number of arguments assigned from the deserialized data will be assigned
      to this address.
      @param[out] ap va_list_t Reference to variable argument list containing
      address where deserialized data should be assigned.
      @returns bool true if the data was successfully decoded, false otherwise.
     */
    bool decode_data(rapidjson::Value &data, const int allow_realloc,
                     size_t *nargs, utils::va_list_t &ap) const override;


};
}
}
}