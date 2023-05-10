#include <boost/python.hpp>
#include <boost/python/enum.hpp>
#include "utils/Address.hpp"
//#include "utils/serialization.hpp"
#include "python/pyYggComm.hpp"

namespace bp = boost::python;
namespace communication {
void (utils::Address::*setaddress)(const std::string &) = &utils::Address::address;
const std::string &(utils::Address::*getaddress)() const = &utils::Address::address;
//void (utils::Metadata::*fromMetadataCopy)(const utils::Metadata&, bool) = &utils::Metadata::fromMetadata;
//void (utils::Metadata::*fromMetadataChar)(const char*, const size_t&, bool) = &utils::Metadata::fromMetadata;
//void (utils::Metadata::*fromMetadataString)(const std::string&, bool) = &utils::Metadata::fromMetadata;
//bool (utils::Metadata::*SetMetaIDChar)(const std::string&, const char**) = &utils::Metadata::SetMetaID;
//bool (utils::Metadata::*SetMetaIDStr)(const std::string&, std::string&) = &utils::Metadata::SetMetaID;
////int (utils::Metadata::*deserializeArgs)(const char*, size_t, int, ...) = &utils::Metadata::deserialize;
//int (utils::Metadata::*deserializeJson)(const char*, rapidjson::VarArgList&) = &utils::Metadata::deserialize;
////int (utils::Metadata::*serializeArgs)(char**, size_t*, size_t, ...) = &utils::Metadata::serialize;
//int (utils::Metadata::*serializeJson)(char**, size_t*, rapidjson::VarArgList&) = &utils::Metadata::serialize;
//void (utils::Metadata::*fromSchemaJson)(const rapidjson::Value&, bool, bool) = &utils::Metadata::fromSchema;
//void (utils::Metadata::*fromSchemaString)(const std::string&, bool) = &utils::Metadata::fromSchema;

void python::exportUtils() {
    bp::object modModule(bp::handle<>(bp::borrowed(PyImport_AddModule("pyYggComm.utils"))));
    bp::scope().attr("module") = modModule;
    bp::scope modScope = modModule;
    /*bp::class_<utils::Metadata, boost::noncopyable>("Metadata", bp::init<>())
            .def("_init", &utils::Metadata::_init)
            .def("fromSchema", fromSchemaJson)
            .def("Normalize", &utils::Metadata::Normalize)
            .def("fromSchema", fromSchemaString)
            .def("fromType", &utils::Metadata::fromType)
            .def("fromScalar", &utils::Metadata::fromScalar)
            .def("fromNDArray", &utils::Metadata::fromNDArray)
            .def("_fromNDArray", &utils::Metadata::_fromNDArray)
            .def("fromFormat", &utils::Metadata::fromFormat)

            .def("fromMetadata", fromMetadataCopy)
            .def("fromMetadata", fromMetadataChar)
            .def("fromMetadata", fromMetadataString)

            .def("fromEncode", &utils::Metadata::fromEncode)
            //.def("GetAllocator", &utils::Metadata::GetAllocator, bp::return_value_policy<bp::copy_const_reference>())
            .def("isGeneric", &utils::Metadata::isGeneric)
            .def("setGeneric", &utils::Metadata::setGeneric)
            .def("empty", &utils::Metadata::empty)
            .def("hasType", &utils::Metadata::hasType)
            .def("hasSubtype", &utils::Metadata::hasSubtype)
            .def("typeName", &utils::Metadata::typeName)
            .def("subtypeName", &utils::Metadata::subtypeName)
            .def("initSchema", &utils::Metadata::initSchema)
            .def("initMeta", &utils::Metadata::initMeta)
            .def("addItem", &utils::Metadata::addItem)
            .def("addMember", &utils::Metadata::addMember)
            //.def("getMeta", &utils::Metadata::getMeta, bp::return_value_policy<bp::copy_const_reference>())
            //.def("getSchema", &utils::Metadata::getSchema, bp::return_value_policy<bp::copy_const_reference>())
            .def("SetValue", &utils::Metadata::SetValue)
            .def("GetInt", &utils::Metadata::GetInt)
            .def("GetIntOptional", &utils::Metadata::GetIntOptional)
            .def("SetInt", &utils::Metadata::SetInt)
            .def("GetMetaInt", &utils::Metadata::GetMetaInt)
            .def("GetMetaIntOptional", &utils::Metadata::GetMetaIntOptional)
            .def("SetMetaInt", &utils::Metadata::SetMetaInt)
            .def("GetSchemaIntOptional", &utils::Metadata::GetSchemaIntOptional)
            .def("SetSchemaInt", &utils::Metadata::SetSchemaInt)
            .def("GetUint", &utils::Metadata::GetUint)
            .def("GetUintOptional", &utils::Metadata::GetUintOptional)
            .def("SetUint", &utils::Metadata::SetUint)
            .def("GetMetaUint", &utils::Metadata::GetMetaUint)
            .def("GetMetaUintOptional", &utils::Metadata::GetMetaUintOptional)
            .def("SetMetaUint", &utils::Metadata::SetMetaUint)
            .def("GetSchemaUintOptional", &utils::Metadata::GetSchemaUintOptional)
            .def("SetSchemaUint", &utils::Metadata::SetSchemaUint)
            .def("GetBool", &utils::Metadata::GetBool)
            .def("GetBoolOptional", &utils::Metadata::GetBoolOptional)
            .def("SetBool", &utils::Metadata::SetBool)
            .def("GetMetaBool", &utils::Metadata::GetMetaBool)
            .def("GetMetaBoolOptional", &utils::Metadata::GetMetaBoolOptional)
            .def("SetMetaBool", &utils::Metadata::SetMetaBool)
            .def("GetSchemaBoolOptional", &utils::Metadata::GetSchemaBoolOptional)
            .def("SetSchemaBool", &utils::Metadata::SetSchemaBool)
            .def("GetString", &utils::Metadata::GetString)
            .def("GetStringOptional", &utils::Metadata::GetStringOptional)
            .def("SetString", &utils::Metadata::SetString)
            .def("GetMetaString", &utils::Metadata::GetMetaString)
            .def("GetMetaStringOptional", &utils::Metadata::GetMetaStringOptional)
            .def("SetMetaString", &utils::Metadata::SetMetaString)
            .def("GetSchemaStringOptional", &utils::Metadata::GetSchemaStringOptional)
            .def("SetSchemaString", &utils::Metadata::SetSchemaString)
            .def("SetMetaValue", &utils::Metadata::SetMetaValue)
            .def("SetSchemaValue", &utils::Metadata::SetSchemaValue)
            .def("SetSchemaMetadata", &utils::Metadata::SetSchemaMetadata)
            .def("SetMetaID", SetMetaIDChar)
            .def("SetMetaID", SetMetaIDStr)
            //.def("deserialize", deserializeArgs)
            .def("deserialize", deserializeJson)
            //.def("serialize", serializeArgs)
            .def("serialize", serializeJson)
            .def("Display", &utils::Metadata::Display);

    bp::class_<utils::Header, bp::bases<utils::Metadata>, boost::noncopyable>("Header", bp::init<>())
            .def("isValid", &utils::Header::isValid)
            .def("invalidate", &utils::Header::invalidate)
            .def("setMessageFlags", &utils::Header::setMessageFlags)
            .def("for_send", &utils::Header::for_send)
            .def("for_recv", &utils::Header::for_recv)
            .def("formatBuffer", &utils::Header::formatBuffer)
            .def("format", &utils::Header::format)
            .def("finalize_recv", &utils::Header::finalize_recv);
*/
    bp::class_<utils::Address>("Address", bp::init<std::string>())
            .def(bp::init<utils::Address *>())
            .def("__str__", &utils::Address::print)
            .def("address", setaddress)
            .def("address", getaddress, bp::return_value_policy<bp::copy_const_reference>())
            .def("key", &utils::Address::key)
            .def("valid", &utils::Address::valid);
    }
}
