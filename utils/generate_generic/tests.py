import pytest
import pdb
import pprint
from contextlib import contextmanager
from generate_generic.base import ContextToken
from generate_generic.cpp import (
    CXXFileUnit, CXXMethodUnit, CMacroUnit, CXXTypedefUnit, CXXTypeUnit,
    CXXFunctionUnit, CXXReturnMacro)


def test_macros():
    snip = (
        """
#define RAPIDJSON_DEFAULT_ALLOCATOR ::RAPIDJSON_NAMESPACE::MemoryPoolAllocator< ::RAPIDJSON_NAMESPACE::CrtAllocator >

#define MACRO_NOARGS HERE

#define MACRO_ARGS(T, name) HERE

#define MACRO_NOARGS_MULTILINE\t\
\tHERE1\t\
\tHERE2

#define MACRO_ARGS_MULTILINE(T, name)\t\
    HERE1\
    HERE2
        """
    )
    res = CXXFileUnit.parse(snip)
    assert res
    pprint.pprint(res.properties)
    assert len(res.properties['members']) == 5
    for x in res.properties['members']:
        pprint.pprint(x.properties)


def test_macro():

    snip = (
        """
#define YGG_GENERIC_HELPER(T, name)
        """
    )
    res = CMacroUnit.parse(snip)
    assert res


def test_type():
    snip = 'typename internal::MaybeAddConst<Const,PlainType>::Type'
    res = CXXTypeUnit.parse(snip)
    assert res


def test_typedef():
    snip = (
        """
typedef Value ValueType;
        """
    )
    res = CXXTypedefUnit.parse(snip)
    assert res
    snip = (
        """
typedef typename internal::MaybeAddConst<Const,PlainType>::Type ValueType;
        """
    )
    res = CXXTypedefUnit.parse(snip)
    assert res


# def test_return_macro():
    
#     snip = (
#         """
#         RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>, internal::IsGenericValue<T> >), (GenericValue&))
#         """
#     )
#     res = CXXReturnMacro.parse(snip, contiguous=True)
#     assert res
#     pprint.pprint(res.properties)
#     assert res.properties['retmacro'].startswith(
#         'RAPIDJSON_DISABLEIF_RETURN')
#     print(res.properties['type'])
#     pprint.pprint(res.properties['type'].properties)
#     assert res.properties['type'] == res.code_unit('type').parse(
#         'GenericValue&')
    
#        RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>, internal::IsGenericValue<T> >), (GenericValue&))

def test_method():

    
    snip = (
        """
        template <typename T>
        GenericValue&
        AddMember(GenericValue& name, T value, Allocator& allocator) {
          GenericValue v(value);
          return AddMember(name, v, allocator);
        }
        """
    )
    kws = {'disable_api': True, 'parent': 'dummy'}
    res = CXXMethodUnit.parse(snip, **kws)
    try:
        assert res
        pprint.pprint(res.properties)
        assert res.properties['name'] == 'AddMember'
    except BaseException:
        import re
        def try_regex(name, regex):
            pattern = re.compile(regex, flags=re.MULTILINE)
            match = pattern.match(snip)
            if match:
                if match.group('name') == 'AddMember':
                    print(f"{name}: SUCCESS!!")
                else:
                    print(f"{name}: MISMATCH:\n{match.group()}")
            else:
                print(f"{name}: FAIL")
        try_regex("CXXMethodUnit", CXXMethodUnit.get_regex(**kws))
        try_regex("CXXFunctionUnit", CXXFunctionUnit.get_regex(**kws))
        regex = '(?P<template>^\s*{template}\s*)?^(?P<indent>\s*){api}(?P<static>static\s+)?(?P<virtual>virtual\s+)?(?P<friend>friend\s+(?:inline\s+)?)?(?P<inline>inline\s+)?(?P<type>{NG:type})\s+(?P<name>(?!operator)\w+)(?:\((?P<args>(?:(?:\s*{NG:var}\s*(?:\,))*\s*{NG:var})?)(?:(?:\s*\,)?(?P<va_args>\.\.\.)|(?P<argmacro>RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\(.+?\)))?\s*\))(?P<const_method>\s+const)?(?P<override>\s+(?:(?:override)|(?:VIRT\_END)))?(?:\s+\w+)?\s*[;\{]'
        # print(CXXMethodUnit._regex_fstring)
        regex = CXXMethodUnit.get_regex(alt_regex_fstring=regex, **kws)
        try_regex("Partial", regex)
        # import pdb; pdb.set_trace()
        raise


def test_class():
#        template <typename T> RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>, internal::IsGenericValue<T> >), (GenericObject)) AddMember(StringRefType name, T value, AllocatorType& allocator) const { value_.AddMember(name, value, allocator); return *this; }
    snip = (
        """
#if RAPIDJSON_HAS_STDSTRING
        //! Add a string object as member (name-value pair) to the object.
        /*! \\param name A string value as name of member.
            \\param value constant string reference as value of member.
            \\param allocator    Allocator for reallocating memory. It must be the same one as used before. Commonly use GenericDocument::GetAllocator().
            \\return The value itself for fluent API.
            \\pre  IsObject()
            \\note This overload is needed to avoid clashes with the generic primitive type AddMember(GenericValue&,T,Allocator&) overload below.
            \\note Amortized Constant time complexity.
         */
        class GenericValue {
        public:
            GenericValue& AddMember(GenericValue& name, std::basic_string<Ch>& value, Allocator& allocator) {
                        GenericValue v(value, allocator);
                        return AddMember(name, v, allocator);
                    }
#ifdef RAPIDJSON_YGGDRASIL
        template <typename T>
        RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>,
                                    internal::OrExpr<internal::IsGenericValue<T>,
                                    YGGDRASIL_IS_SCALAR_TYPE(T) > >),
                                   (GenericValue&))
#else // RAPIDJSON_YGGDRASIL
        template <typename T>
        RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>, internal::IsGenericValue<T> >), (GenericValue&))
#endif // RAPIDJSON_YGGDRASIL
        AddMember(GenericValue& name, T value, Allocator& allocator) {
          GenericValue v(value);
          return AddMember(name, v, allocator);
        }
#endif


        //! Append a primitive value at the end of the array.
        /*! \tparam T Either \ref Type, \c int, \c unsigned, \c int64_t, \c uint64_t
            \param value Value of primitive type T to be appended.
            \param allocator    Allocator for reallocating memory. It must be the same one as used before. Commonly use GenericDocument::GetAllocator().
            \pre IsArray() == true
            \return The value itself for fluent API.
            \note If the number of elements to be appended is known, calls Reserve() once first may be more efficient.

            \note The source type \c T explicitly disallows all pointer types,
              especially (\c const) \ref Ch*.  This helps avoiding implicitly
              referencing character strings with insufficient lifetime, use
            \ref PushBack(GenericValue&, Allocator&) or \ref
              PushBack(StringRefType, Allocator&).
              All other pointer types would implicitly convert to \c bool,
                    use an explicit cast instead, if needed.
            \note Amortized constant time complexity.
         */
        template <typename T>
#ifdef RAPIDJSON_YGGDRASIL
        RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>,
                                    internal::OrExpr<internal::IsGenericValue<T>,
                                    YGGDRASIL_IS_SCALAR_TYPE(T) > >),
                                   (GenericValue&))
#else // RAPIDJSON_YGGDRASIL
        RAPIDJSON_DISABLEIF_RETURN((internal::OrExpr<internal::IsPointer<T>, internal::IsGenericValue<T> >), (GenericValue&))
#endif // RAPIDJSON_YGGDRASIL
        PushBack(T value, Allocator& allocator) {
          GenericValue v(value);
          return PushBack(v, allocator);
        }
        };
        """
    )
    remove_token = ContextToken(
        r'RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\_RETURN\('
        r'\s*\([^\{]+(?:\n[^\{]*)*\)\s*\,\s*\(\s*',
        r'\s*\)\s*\)(?:[ \t]*$\n)?',
        name='return_macro', use_regex=True, recursive=False,
        exclusive=True,
    )
    match = remove_token.find(snip)
    snip_pruned = remove_token.removeall(snip)
    print(snip_pruned)
    print(match)
    try:
        assert match
        assert 'RAPIDJSON_DISABLEIF_RETURN' not in snip_pruned
    except BaseException:
        import pdb; pdb.set_trace()
        raise
    res = CXXFileUnit.parse(snip, disable_api=True, name='test_file',
                            preprocess_kws=dict(
                                remove_tokens=[remove_token]))
    assert res
    pprint.pprint(res.properties)
    assert res.get_property('members', None)
    assert len(res.properties['members']) == 1
    pprint.pprint(res['GenericValue'].properties)
    assert res['GenericValue'].get_property('members', None)
    assert len(res['GenericValue'].properties['members']) == 3
    method = res['GenericValue'].properties['members'][1]
    pprint.pprint(method.properties)
    assert method.properties['name'] == 'AddMember'
    assert len(method.properties['args']) == 3
    method = res['GenericValue'].properties['members'][2]
    pprint.pprint(method.properties)
    assert method.properties['name'] == 'PushBack'
    assert len(method.properties['args']) == 2
