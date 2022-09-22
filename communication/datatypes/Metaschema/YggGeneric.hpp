#pragma once

#include <cstdlib>
#include <string>
#include <map>
#include <vector>
#include "Dict.hpp"

namespace communication {
namespace datatypes {
namespace Metaschema {

class MetaschemaType;

class YggGeneric {
private:
    MetaschemaType *type;
    Dict *data;
    size_t nbytes;
public:
    YggGeneric();
    /*!
      @brief Constructor.
      @param[in] in_type MetaschemaType* Pointer to type class describing data.
      @param[in] in_data void* Pointer to data.
      @param[in] in_nbytes size_t Number of bytes at the address provided
      by data. Defaults to 0 and will be set by type->nbytes().
     */
    YggGeneric(const MetaschemaType* in_type, Dict* in_data, size_t in_nbytes=0);
    /*!
      @brief Copy constructor.
      @param[in] other YggGeneric Instance of class to copy.
     */
    YggGeneric(const YggGeneric &other);
    YggGeneric(const YggGeneric* other);
    /*!
      @brief Desctructor.
     */
    ~YggGeneric();
    /*!
      @brief Display the data.
     */
    void display(const char* indent="") const;
    /*!
      @brief Get a copy of the data.
      @param[in] orig_data Pointer to data that should be copied if different
      that the data that is wrapped.
      @returns void* Pointer to copy of data.
    */
    void* copy_data(Dict* orig_data=NULL) const;
    /*!
      @brief Free the memory used by the data.
     */
    void free_data();
    /*!
      @brief Free the memory used by the type.
     */
    void free_type();
    /*!
      @brief Set the data type.
      @param[in] new_type MetaschemaType* Pointer to new type.
     */
    void set_type(const MetaschemaType* new_type);
    /*!
      @brief Get the data type.
      @returns MetaschemaType* Pointer to data type.
     */
    MetaschemaType* get_type() const;
    /*!
      @brief Set the data size.
      @param[in] new_nbytes size_t New data size.
     */
    void set_nbytes(size_t new_nbytes);
    /*!
      @brief Get the data size.
      @returns size_t Number of bytes in the data object.
     */
    size_t get_nbytes() const;
    /*!
      @brief Get a pointer to the data size.
      @returns size_t* Pointer to number of bytes in the data object.
     */
    size_t* get_nbytes_pointer();
    /*!
      @brief Get the number of elements in the data.
      @returns size_t Number of elements in the data.
     */
    size_t get_nelements() const;
    /*!
      @brief Set data.
      @param[in] new_data void* New data.
     */
    void set_data(Dict *new_data);
    /*!
      @brief Extract data.
      @returns void* Pointer to data.
     */
    Dict* get_data() const;
    /*!
      @brief Get the data pointer.
      @returns void** Pointer to data object pointer.
     */
    Dict** get_data_pointer();
    /*!
      @brief Extract data and copy into the provided variable.
      @param[out] obj T* Pointer to existing variable where data should be copied.
      @param[in] nelements size_t Number of elements in the provided array.
      Defaults to 1 if not provided.
      @param[in] is_char bool If True, the input array is treated as a
      charater array and need only be larger than the size of the data.
      Defaults to false if not provided.
     */
    template <typename T>
    void get_data(T* obj, size_t nelements=1, bool is_char=false) const;
    /*!
      @brief Extract data and assign the value to the provided variable.
      @param[out] obj T Existing variable where data should be stored.
     */
    template <typename T>
    void get_data(T &obj) const;
    /*!
      @brief Extract data, realloc provided array, and copy data into it.
      @param[out] obj T** Pointer to existing array where data should be copied.
      @param[in, out] nelements size_t* Pointer to number of elements in
      the provided array. The number of elements in teh array after
      reallocation is stored at this address. Defaults to NULL if not
      provided.
     */
    template <typename T>
    void get_data_realloc(T** obj, size_t* nelements=NULL) const;
    /*!
      @brief Extract data and copy into the provided variable.
      @param[out] obj T* Pointer to existing variable where data should be copied.
      @param[in] nelements size_t Number of elements in the provided array.
      Defaults to 1 if not provided.
     */
    void get_data(char* obj, size_t nelements) const;
    /*!
      @brief Add an element to the end of the array if this object is an
      array.
      @param[in] x YggGeneric* Pointer to new element.
     */
    void add_array_element(YggGeneric *x);
    /*!
      @brief Set an array element if this object is an array.
      @param[in] i size_t Index where element should be assigned.
      @param[in] x YggGeneric* Pointer to new element.
     */
    void set_array_element(size_t i, YggGeneric *x);
    /*!
      @brief Get an array element if this array is an array.
      @param[in] i size_t Index of element that should be returned.
      @returns YggGeneric* Pointer to element at index i.
     */
    YggGeneric* get_array_element(size_t i);
    /*!
      @brief Set an object element if this object is an object.
      @param[in] k const char* Key where element should be assigned.
      @param[in] x YggGeneric* Pointer to new element.
     */
    void set_object_element(const char *k, YggGeneric *x);
    /*!
      @brief Get an object element if this object is an object.
      @param[in] k const char* Key of element that should be returned.
      @returns YggGeneric* Pointer to element at index i.
     */
    YggGeneric* get_object_element(const char *k);
    /*!
      @brief Get data and check against a data type.
      @param[in] exp_type MetaschemaType* Type that is expected to be
      returned.
     */
    void* get_data(const MetaschemaType* exp_type) const;
    /*!
      @brief Get the size of an array.
      @returns size_t Size of the wrapped array.
     */
    size_t get_data_array_size() const;
    /*!
      @brief Get the size of a map.
      @returns size_t Size of the wrapped map.
     */
    size_t get_data_map_size() const;
    /*!
      @brief Determine if a map contains a specific key.
      @param[in] key char* Key to check for.
      @returns bool true if the key is present, false otherwise.
     */
    bool has_data_map_key(char* key) const;
    /*!
      @brief Get an array of keys in the map.
      @param[out] keys char*** Pointer to memory that should be reallocated
      to allow for a storate of character array pointers.
      @returns size_t Number of keys in keys array.
     */
    size_t get_data_map_keys(char*** keys) const;
    /*!
      @brief Get an array item.
      @param[in] i const size_t Index of array element that should be
      returned.
      @param[in] item_type MetaschemaType* Type that should be returned.
      @param[in] return_generic bool If true, the generic wrapper object
      will be returned. Defaults to false.
      @returns void* Pointer to the data.
     */
    void* get_data_array_item(const size_t i,
                              const MetaschemaType* item_type,
                              bool return_generic=false) const;
    /*!
      @brief Get the size of an array item in bytes.
      @param[in] i const size_t Index of array element that should be
      returned.
      @returns size_t Size of the item in bytes.
     */
    size_t get_nbytes_array_item(const size_t i) const;
    /*!
      @brief Set an array item.
      @param[in] index const size_t Index of array element that should
      be set. If larger than the current size of the array, the element
      will be appended to the end of the array.
      @param value const YggGeneric* Pointer to value that the indexed
      item in the array should be set to.
     */
    void set_data_array_item(const size_t index,
                             const YggGeneric* value);
    /*!
      @brief Get a map item.
      @param[in] key const char* String key for item that should be
      returned.
      @param[in] item_type MetaschemaType* Type that should be returned.
      @param[in] return_generic bool If true, the generic wrapper object
      will be returned. Defaults to false.
      @returns void* Pointer to the data.
     */
    void* get_data_map_item(const char *key,
                            const MetaschemaType* item_type,
                            bool return_generic=false) const;
    /*!
      @brief Get the size of a map item in bytes.
      @param[in] key const char* String key for item that should be
      returned.
      @returns size_t Size of the item in bytes.
     */
    size_t get_nbytes_map_item(const char *key) const;
    /*!
      @brief Set a map item.
      @param[in] key const char* String key for item that should be set.
      @param[in] value const YggGeneric* Pointer to value that map item
      should be set to.
     */
    void set_data_map_item(const char *key, const YggGeneric* value);

};

/*! @brief Vector of generic types. */
typedef std::vector<YggGeneric*> YggGenericVector;

/*! @brief Map of generic types. */
typedef std::map<std::string, YggGeneric*> YggGenericMap;
//typedef std::map<const char*, YggGeneric*, strcomp> YggGenericMap;


}
}
}
