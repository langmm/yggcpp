#pragma once

#include "ScalarMetaschemaType.hpp"
#include "utils/tools.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

/*!
  @brief Base class for ND array type definition.

  The NDArrayMetaschemaType provides basic functionality for encoding/decoding
  ND array datatypes from/to JSON style strings.
 */
class NDArrayMetaschemaType : public ScalarMetaschemaType {
public:
    NDArrayMetaschemaType() = delete;

    /*!
      @brief Constructor for NDArrayMetaschemaType.
      @param[in] subtype const character pointer to the name of the subtype.
      @param[in] precision size_t Type precision in bits.
      @param[in] shape std::vector<size_t> Shape of type array in each dimension.
      @param[in] units const char * (optional) Type units.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    NDArrayMetaschemaType(const char *subtype, const size_t precision,
                          const std::vector<size_t> shape, const char *units = "",
                          const bool use_generic = false);

    /*!
      @brief Constructor for NDArrayMetaschemaType from a JSON type defintion.
      @param[in] type_doc rapidjson::Value rapidjson object containing the type
      definition from a JSON encoded header.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    NDArrayMetaschemaType(const rapidjson::Value &type_doc,
                          const bool use_generic = false);

    /*!
      @brief Constructor for NDArrayMetaschemaType from Python dictionary.
      @param[in] pyobj PyObject* Python object.
      @param[in] use_generic bool If true, serialized/deserialized
      objects will be expected to be YggGeneric classes.
     */
    NDArrayMetaschemaType(PyObject *pyobj, const bool use_generic = false);

    /*!
      @brief Copy constructor.
      @param[in] other NDArrayMetaschemaType* Instance to copy.
     */
    NDArrayMetaschemaType(const NDArrayMetaschemaType &other);

    virtual ~NDArrayMetaschemaType();

    /*!
      @brief Equivalence operator.
      @param[in] Ref MetaschemaType instance to compare against.
      @returns bool true if the instance is equivalent, false otherwise.
     */
    bool operator==(const MetaschemaType &Ref) const override;

    /*!
      @brief Create a copy of the type.
      @returns pointer to new NDArrayMetaschemaType instance with the same data.
     */
    NDArrayMetaschemaType *copy() const override;

    /*!
      @brief Print information about the type to stdout.
      @param[in] indent char* Indentation to add to display output.
    */
    void display(const char *indent = "") const override;

    /*!
      @brief Get type information as a Python dictionary.
      @returns PyObject* Python dictionary.
     */
    PyObject *as_python_dict() const override;

    /*!
      @brief Get the number of dimensions in the array.
      @returns size_t Number of dimensions in type.
     */
    const size_t ndim() const;

    /*!
      @brief Get the shape of the array type.
      @returns std::vector<size_t> Shape of type in each dimension.
     */
    std::vector<size_t> shape() const;

    /*!
      @brief Get the number of elements in the type.
      @returns size_t Number of elements.
     */
    const size_t nelements() const override;

    /*!
      @brief Determine if the number of elements is variable.
      @returns bool true if the number of elements can change, false otherwise.
    */
    const bool variable_nelements() const override;

    /*!
      @brief Determine the dimensions of the equivalent numpy array.
      @param[in, out] nd int* Address of integer where number of dimensions should be stored.
      @param[in, out] dims npy_intp** Address of pointer to memory where dimensions should be stored.
     */
    void numpy_dims(int *nd, npy_intp **dims) const override;

    /*!
      @brief Get the number of arguments expected to be filled/used by the type.
      @returns size_t Number of arguments.
     */
    size_t nargs_exp() const override;

    /*!
      @brief Update the type object with info from another type object.
      @param[in] new_info MetaschemaType* type object.
     */
    void update(const MetaschemaType *new_info) override;

    /*!
      @brief Update the type object with info from provided variable arguments for serialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_serialization_args(size_t *nargs, struct va_list_t &ap) override;

    /*!
      @brief Update the type object with info from provided variable arguments for deserialization.
      @param[in,out] nargs size_t Number of arguments contained in ap. On output
      the number of unused arguments will be assigned to this address.
      @param[in] ap va_list_t Variable argument list.
      @returns size_t Number of arguments in ap consumed.
     */
    size_t update_from_deserialization_args(size_t *nargs, struct va_list_t &ap) override;

    /*!
      @brief Update the instance's shape.
      @param[in] new_shape std::vector<size_t> Vector of new array sizes in each dimension.
      @param[in] force bool True if the shape should be updated even if it
      is not compatible with the existing value.
     */
    void set_shape(std::vector<size_t> new_shape, bool force = false);

    /*!
      @brief Encode the type's properties in a JSON string.
      @param[in] writer rapidjson::Writer<rapidjson::StringBuffer> rapidjson writer.
      @returns bool true if the encoding was successful, false otherwise.
     */
    bool encode_type_prop(rapidjson::Writer<rapidjson::StringBuffer> *writer) const override;

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
                     size_t *nargs, struct va_list_t &ap) const override;

private:
    std::vector<size_t> shape_;
    bool _variable_shape;

};
}
}
}
