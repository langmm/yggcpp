#include "AnyMetaschemaType.hpp"

using namespace communication::datatypes::Metaschema;
using namespace communication::utils;

AnyMetaschemaType::AnyMetaschemaType(const bool use_generic, const MetaschemaType *temp_type) :
        MetaschemaType("any", true, use_generic), temp_type_(NULL) {
    if (temp_type != NULL)
        temp_type_ = temp_type->copy();
}

AnyMetaschemaType::AnyMetaschemaType(const rapidjson::Value &type_doc,
                                     const bool use_generic) :
        MetaschemaType(type_doc, true, use_generic), temp_type_(NULL) {
    if (!(type_doc.HasMember("temptype")))
        ygglog_throw_error("AnyMetaschemaType: Parsed header dosn't contain a temptype.");
    if (!(type_doc["temptype"].IsObject()))
        ygglog_throw_error("AnyMetaschemaType: Temporary type in parsed header is not an object.");
    temp_type_ = (MetaschemaType *) type_from_doc_c(&(type_doc["temptype"]), true);
    if (temp_type_ == NULL) {
        ygglog_throw_error("AnyMetaschemaType: Failed to get temporary type from document.");
    }
}

AnyMetaschemaType::~AnyMetaschemaType() {
    if (temp_type_ != NULL) {
        delete temp_type_;
        temp_type_ = NULL;
    }
}

bool AnyMetaschemaType::operator==(const MetaschemaType &Ref) const {
    if (!(MetaschemaType::operator==(Ref)))
        return false;
    const AnyMetaschemaType *pRef = dynamic_cast<const AnyMetaschemaType *>(&Ref);
    if (pRef->temp_type() == NULL) {
        if (temp_type_ != NULL)
            return false;
    } else if (temp_type_ == NULL) {
        if (pRef->temp_type() != NULL)
            return false;
    } else {
        if ((*(pRef->temp_type())) != (*temp_type_))
            return false;
    }
    return true;
}

bool AnyMetaschemaType::operator!=(const AnyMetaschemaType &Ref) const {
    if (operator==(Ref))
        return false;
    else
        return true;
}

void AnyMetaschemaType::display(const char *indent) const {
    MetaschemaType::display(indent);
    if (temp_type_ != NULL) {
        printf("%s%-15s = %s\n", indent, "temptype", "");
        char new_indent[100] = "";
        strcat(new_indent, indent
        );
        strcat(new_indent,
               "    ");
        temp_type_->
                display(new_indent);
    }
}

PyObject* AnyMetaschemaType::as_python_dict() const {
    PyObject *out = MetaschemaType::as_python_dict();
    PyObject *py_temp_type = temp_type_->as_python_dict();
    set_item_python_dict(out,
                         "temptype", py_temp_type,
                         "AnyMetaschemaType::as_python_dict: temptype: ",
                         T_OBJECT);
    return out;
}

void AnyMetaschemaType::update(const MetaschemaType *new_info) {
    if (temp_type_ != NULL) {
        delete temp_type_;
        temp_type_ = NULL;
    }
    MetaschemaType::update(new_info);
    const AnyMetaschemaType *new_info_any = dynamic_cast<const AnyMetaschemaType *>(new_info);
    temp_type_ = new_info_any->temp_type()->copy();
}

size_t AnyMetaschemaType::update_from_serialization_args(size_t *nargs, va_list_t &ap) {
    size_t out = MetaschemaType::update_from_serialization_args(nargs, ap);
    if (use_generic())
        return out;
    if (temp_type_ == NULL) {
        ygglog_throw_error("AnyMetaschemaType::update_from_serialization_args: Temp type is NULL.");
    }
    return temp_type_->update_from_serialization_args(nargs, ap);
}


