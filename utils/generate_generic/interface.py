import os
import copy
import pdb
import pprint
from contextlib import contextmanager
from generate_generic import GeneratedFile
from generate_generic.base import get_file_unit, ContextToken
from generate_generic.cpp import (
    CXXModuleUnit, CXXFunctionUnit, CXXTypeUnit, CXXTypedefUnit,
    CImportUnit, CMacroUnit)


# TODO:
# - Add field unit?
# - Add property unit with setter/getter
# - Add ObjWavefront, Ply, GenericQuantity, GenericQuantityArray
# - Handle separation of C/CXX header/source
# - Get docs by looking backwards for comments


_interface_file_registry = {
    'classes': {},
    'instances': {},
}


def register_interface_file(k, v, instance=False):
    if k is None:
        return
    global _interface_file_registry
    category = 'instances' if instance else 'classes'
    _interface_file_registry[category][k] = v


def get_interface_file(k, instance=False, **kwargs):
    global _interface_file_registry
    category = 'instances' if instance else 'classes'
    if instance and k not in _interface_file_registry[category]:
        inst = get_interface_file(k)(**kwargs)
        register_interface_file(k, inst, instance=True)
    return _interface_file_registry[category][k]


class InterfaceFileMeta(type):

    def __new__(meta, name, bases, class_dict):
        cls = type.__new__(meta, name, bases, class_dict)
        register_interface_file(cls.name, cls)
        return cls


class GeneratedInterfaceFileMeta(type):

    def __new__(meta, name, bases, class_dict):
        cls = type.__new__(meta, name, bases, class_dict)
        register_interface_file(cls.name, cls)
        return cls


class RemoveMemberError(RuntimeError):

    def __init__(self, member, *args, **kwargs):
        self.member = member
        super(RemoveMemberError, self).__init__(*args, **kwargs)


class InterfaceFileBase(metaclass=InterfaceFileMeta):

    name = None
    disabled = False
    filename = None
    language = None
    module = []
    disable_api = False
    ignored_preprocess_contexts = [
        'def RAPIDJSON_YGGDRASIL',
    ]
    depends_on = []
    _provided_types = []
    _related_files = []
    modification_types = [
        'selected_members', 'removed_members', 'modified_members',
        'added_members', 'added_properties',
        'removed_properties', 'added_containers', 'replaced_types',
        'final_modified_members',
    ]

    def __init__(self, related_files=None, filename=None,
                 modifications=None, modification_functions=None,
                 **kwargs):
        self.unit = self.load(filename=filename, **kwargs)
        self.name_changes = {}
        if modifications is None:
            modifications = {}
        if modification_functions is None:
            modification_functions = {}
        self.external = modifications
        self.external_functions = modification_functions
        if related_files is None:
            related_files = {}
        self.related_files = related_files
        for x in (self._related_files + self.depends_on):
            if x not in self.related_files:
                self.related_files[x] = get_interface_file(
                    x, instance=True, **kwargs)
                self.related_files.update(
                    **self.related_files[x].related_files)
        if not self.disabled:
            self.make_modifications()

    def make_modifications(self):
        for k in self.modification_types:
            method = getattr(self, f'do_{k}')
            for k, v in self.get_modification(k).items():
                try:
                    for x in self._get_units(k):
                        try:
                            method(k, v, x)
                        except RemoveMemberError as e:
                            e.member.get_property(
                                'parent_unit').remove_members(
                                    [e.member])
                except BaseException as e:
                    print(f"Error in modification {k} ({v}): {e}")
                    pdb.set_trace()
                    raise

    def __repr__(self):
        return f"{self.__class__.__name__}({self.name})"

    @contextmanager
    def _checking_alias(self, unit, required=False):
        old_key = self._unit_key(unit)
        yield
        new_key = self._unit_key(unit)
        if required or old_key != new_key:
            self._add_alias(old_key, new_key)

    @classmethod
    def _unit_key(cls, unit):
        return unit.address_tuple[1:-1]

    def _add_alias(self, a, b):
        assert a not in self.name_changes
        assert a != b
        self.name_changes[a] = b

    def _get_alias(self, key):

        def key_startswith(a, b):
            assert isinstance(a, tuple)
            assert isinstance(b, tuple)
            for aa, bb in zip(a, b):
                if ((aa != bb
                     and (None not in [aa, bb])
                     and (True not in [aa, bb]))):
                    return False
            return True

        out = key
        for k, v in self.name_changes:
            if key_startswith(out, k):
                out = tuple(
                    [vv if ((None not in [vv, out[i]])
                            and (True not in [vv, out[i]])) else out[i]
                     for i, vv in enumerate(v)]
                    + list(out)[len(k):])
        return out

    def _get_units(self, key, required_alias=False):
        out = self.unit
        if key is True:
            out = list(out.iter_child_members())
        elif key is not None:
            if isinstance(key, str):
                key = (key, )
            assert isinstance(key, tuple)
            key = self._get_alias(key)
            for k in key:
                if k is None:
                    if isinstance(out, list):
                        tmp = []
                        for x in out:
                            tmp += x.properties.get('members', [])
                        out = tmp
                    else:
                        out = out.properties.get('members', [])
                else:
                    assert isinstance(k, str)
                    if isinstance(out, list):
                        out = [x[k] for x in out]
                    else:
                        try:
                            out = out[k]
                        except BaseException:
                            print("HERE", out, k)
                            pprint.pprint(out.properties)
                            raise
        if not isinstance(out, list):
            out = [out]
        for x in out:
            with self._checking_alias(x, required=required_alias):
                yield x

    def _get_unit(self, key):
        out = list(self._get_units(key))
        assert len(out) == 1
        return out[0]

    @classmethod
    def append_removed_member(cls, reg, v):
        key = cls._unit_key(v)
        reg.setdefault(key, {})
        reg[key].setdefault('members', [])
        reg[key]['members'].append(v)

    def append_modification(self, name, old, new):
        if isinstance(old, list):
            assert isinstance(new, list)
            old += new
        else:
            assert isinstance(old, dict)
            assert isinstance(new, dict)
            for k, v in new.items():
                if k in old:
                    # TODO: Concatenate
                    pprint.pprint(old[k])
                    pprint.pprint(v)
                    raise Exception(f"CONCATENATE[{name}]: {k}: "
                                    f"{type(old[k])}, "
                                    f"{type(v)}")
                else:
                    old[k] = v

    def get_modification(self, name, skip_property=False):
        out = copy.deepcopy(getattr(self, f'_{name}', {}))
        if (not skip_property) and hasattr(self, name):
            self.append_modification(name, out, getattr(self, name))
        if name in self.external:
            self.append_modification(name, out, self.external[name])
        if name in self.external_functions:
            out = self.external_functions[name](self, out)
        return out

    @property
    def provided_types(self):
        return self.get_modification('provided_types', skip_property=True)

    def do_selected_members(self, k, v, x):
        orig_members = [xx for xx in x.properties.get('members', [])]
        x.select_members(**v)
        for name in v.get('member_names', []):
            if x.has_member(name):
                continue
            # orig_members[5].properties['body']
            msg = f"Could not locate member: {name}"
            print(msg)
            pprint.pprint(orig_members)
            pdb.set_trace()
            raise Exception(msg)

    def do_removed_members(self, k, v, x):
        x.remove_members(**v)

    def do_modified_members(self, k, v, x):
        if isinstance(v, list):
            for vv in v:
                vv(x)
        else:
            v(x)

    def do_final_modified_members(self, k, v, x):
        self.do_modified_members(k, v, x)

    def do_added_members(self, k, v, x):
        x.add_member(v, make_copy=True)

    def do_added_properties(self, k, v, x):
        for kk, vv in v.items():
            x.set_property(kk, vv)

    def do_removed_properties(self, k, v, x):
        for vv in v:
            x.properties.pop(vv, None)

    def do_added_containers(self, k, v, x):
        x.add_container(v[0], **v[1])

    def do_replaced_types(self, k, v, x):
        for kk, vv in v.items():
            x.replace_type(kk, vv)

    @property
    def removed_members(self):
        out = {}
        for x in self.depends_on:
            if self.related_files[x].disabled:
                out.setdefault(True, {'member_types': []})
                out[True]['member_types'] += self.related_files[
                    x].provided_types
        return out

    @property
    def added_properties(self):
        out = {}
        out.setdefault(None, {})
        out[None]['ignored_preprocess_contexts'] = (
            self.ignored_preprocess_contexts)
        out.setdefault(True, {})
        out[True]['file_unit'] = self.unit.get_property('file_unit')
        return out

    @classmethod
    def load(cls, filename=None, **kwargs):
        if cls.disabled:
            kwargs['dont_read'] = True
        if filename is None:
            filename = cls.filename
        unit = get_file_unit(filename, language=cls.language,
                             disable_api=cls.disable_api, **kwargs)
        return unit

    @classmethod
    def _remove_templated_methods(cls, unit):
        if unit.unit_type != 'class':
            return
        members = [
            x for x in unit.properties.pop('members', [])
            if ((x.unit_type not in ['method', 'operator', 'constructor']
                 or x.properties.get('template', None) is None))
        ]
        unit.add_members(members)


class RapidjsonInterfaceFileMixin:

    language = 'cxx'
    disable_api = True

    def __init__(self, **kwargs):
        self.wrap_rapidjson = kwargs.get('wrap_rapidjson', False)
        super().__init__(**kwargs)

    @classmethod
    def load(cls, filename=None, rapidjson_include_dirs=None,
             preprocess_kws=None, **kwargs):
        if filename is None:
            filename = cls.filename
        if preprocess_kws is None:
            preprocess_kws = {}
        preprocess_kws.setdefault('remove_tokens', [])
        preprocess_kws['remove_tokens'] = preprocess_kws['remove_tokens'] + [
            ContextToken(
                r'RAPIDJSON\_(?:(?:DIS)|(?:EN))ABLEIF\_RETURN\('
                r'\s*\([^\{]+(?:\n[^\{]*)*\)\s*\,\s*\(\s*',
                r'\s*\)\s*\)(?:[ \t]*$\n)?',
                name='return_macro', use_regex=True, recursive=False,
                exclusive=True,
            )
        ]
        if rapidjson_include_dirs is None:
            rapidjson_include_dirs = os.path.join(
                'cpp', 'include', 'rapidjson', 'include')
        if not os.path.isabs(filename):
            filename = os.path.join(rapidjson_include_dirs, filename)
        return super().load(filename=filename,
                            preprocess_kws=preprocess_kws,
                            **kwargs)


class RapidjsonInterfaceFileBase(RapidjsonInterfaceFileMixin,
                                 InterfaceFileBase):

    _replaced_types = {
        None: {
            'SizeType': 'unsigned',
        },
    }
    _added_containers = {
        None: ('module', {'name': 'rapidjson'}),
    }


class RapidjsonDocumentFile(RapidjsonInterfaceFileBase):

    name = 'rapidjson_document'
    filename = os.path.join('rapidjson', 'document.h')
    depends_on = [
        'rapidjson_enum', 'rapidjson_ply', 'rapidjson_obj',
        'rapidjson_units',
    ]
    _provided_types = [
        'GenericValue', 'Value',
        'GenericDocument', 'Document',
        'GenericArray', 'Array', 'ConstArray',
        'GenericObject', 'Object', 'ConstObject',
        'GenericMember', 'Member',
        'GenericMemberIterator', 'MemberIterator', 'ConstMemberIterator',
    ]
    _replaced_types = {
        None: {
            'SizeType': 'unsigned',
        },
    }
    _removed_members = {
        (None, ): {
            'member_names': [
                'v', 'data',
            ],
        },
    }
    _selected_members = {
        None: {
            'member_names': [
                'GenericValue', 'GenericDocument',
                'GenericArray', 'GenericObject',
                # 'GenericMember',
            ],
        }
    }
    template_spec = {
        'scalar': [
            'float16_t', 'uint8_t', 'uint16_t', 'int8_t', 'int16_t',
            'std::complex<float>', 'std::complex<double>',
        ],
        'json': [
            'float', 'double',
            'uint32_t', 'uint64_t', 'int32_t', 'int64_t',
        ],
        'json_alloc': [
            'std::string',
        ],
    }
    method_template_spec = {
        'PushBack': {
            'exclude': ['std::string'],
        },
        'AddMember': {
            'exclude': ['std::string'],
        },
    }

    @property
    def removed_members(self):
        out = super(RapidjsonDocumentFile, self).removed_members
        for x in self.unit.properties['members']:
            for m in x.properties.get('members', []):
                if ((m.unit_type == 'constructor'
                     and len(m.properties['args']) == 1)):
                    m_type = m.properties['args'][0].properties['type']
                    if m_type.properties.get('ptr', '').strip() == '&&':
                        self.append_removed_member(out, m)
        return out

    @classmethod
    def _wrap_value_returns(cls, unit):
        if not (unit.unit_type == 'method'
                and (unit.properties['type'].properties[
                    'base'].properties['name'] in
                     ['GenericValue', 'ValueType', 'Value',
                      'GenericDocument', 'Document', 'GenericMember',
                      'GenericQuantityArray', 'GenericQuantity',
                      'QuantityArray', 'Quantity'])):
            return
        name = unit.properties['name']
        if name in ['GetTitle', 'GetUnits', 'YggSubTypeString',
                    'GetYggType', 'GetSubType', 'GetEncoding']:
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'std::basic_string<Ch>')
            unit.properties['wrapped_type_conv'] = '.GetString()'
        elif name in ['GetShape']:
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'std::vector<SizeType>')
            unit.properties['wrapped_type_conv'] = (
                 '.GetVector<SizeType>()')
        elif (name in ['Swap', 'PopBack', 'PushBack', 'AddMember',
                       'Parse', 'CopyFrom', 'CopyInto', 'MemberReserve',
                       'Reserve', 'floor_inplace']
              or name.startswith('Set')):
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'void')

    @classmethod
    def _wrap_returned_references(cls, unit):
        if not (unit.unit_type == 'method'
                and (unit.properties['type'].properties[
                    'base'].properties['name'] in
                     ['GenericValue', 'ValueType', 'Value',
                      'GenericDocument', 'Document', 'GenericMember',
                      'GenericQuantityArray', 'GenericQuantity',
                      'QuantityArray', 'Quantity'])):
            return
        m_type = unit.properties['type']
        if '&' in m_type.properties.get('ptr', ''):
            m_type.properties['ptr'] = m_type.properties[
                'ptr'].replace('&', '')

    @classmethod
    def _specialize_method_template(cls, unit):
        template = unit.properties.get('template', None)
        if not (template and unit.unit_type in ['method', 'operator',
                                                'constructor']):
            return
        ValueType = CXXTypeUnit.parse(
            'GenericValue<UTF8<>,AllocatorType>'
        )
        AllocatorType = CXXTypeUnit.parse('AllocatorType')
        if ((len(template.properties.get('param', [])) == 1
             and (template.properties['param'][0].properties['name']
                  == 'SourceAllocator'))):
            spec = {
                'SourceAllocator': AllocatorType,
            }
            unit.specialize(unit.address_local, spec)
        elif ((len(template.properties.get('param', [])) == 1
               and (template.properties['param'][0].properties['name']
                    == 'YggSchemaValueType'))):
            spec = {
                'YggSchemaValueType': ValueType,
            }
            unit.specialize(unit.address_local, spec)
        elif ((len(template.properties.get('param', [])) == 1
               and (template.properties['param'][0].properties['name'] in
                    ['T', 'T2', 'Ch2', 'DestEncoding']))):
            parm_name = template.properties['param'][0].properties['name']
            if parm_name == 'Ch2':
                spec_types = ['Ch']
            elif parm_name == 'DestEncoding':
                spec_types = ['UTF8<>']
            else:
                spec_types = copy.deepcopy(cls.template_spec['json'])
            has_alloc = any(x.properties['name'] == 'allocator'
                            for x in unit.properties['args'])
            has_units = any((x.properties['name'] == 'units'
                             or x.properties['name'] == 'units_str')
                            for x in unit.properties['args'])
            has_shape = any(x.properties['name'] == 'shape'
                            for x in unit.properties['args'])
            has_len = any(x.properties['name'] == 'nelements'
                          for x in unit.properties['args'])
            if has_alloc:
                if not (has_units or has_shape or has_len):
                    spec_types += cls.template_spec['json_alloc']
                spec_types += cls.template_spec['scalar']
            method_spec_info = cls.method_template_spec.get(
                unit.properties['name'], {})
            spec_types += method_spec_info.get('include', [])
            for x in method_spec_info.get('exclude', []):
                if x in spec_types:
                    spec_types.remove(x)
            spec = {parm_name: [CXXTypeUnit.parse(x) for x in spec_types]}
            unit.specialize(unit.address_local, spec)
        elif any(x.properties.get('param_type', None) != 'typename'
                 for x in template.properties.get('param', [])):
            raise RemoveMemberError(unit)

    @classmethod
    def _specialize_class_template(cls, unit):
        if not ('template' in unit.properties
                and unit.properties['name'].startswith('Generic')):
            return
        ValueType = CXXTypeUnit.parse('Value')
        EncodingType = CXXTypeUnit.parse('UTF8<>')
        AllocatorType = CXXTypeUnit.parse('RAPIDJSON_DEFAULT_ALLOCATOR')
        StackAllocatorType = CXXTypeUnit.parse(
            'RAPIDJSON_DEFAULT_STACK_ALLOCATOR')
        name_base = unit.properties['name'].split('Generic')[-1]
        new_name = name_base
        kws = {}
        instant_spec = {}
        if unit.properties['name'].endswith(('Quantity',
                                             'QuantityArray',
                                             'Units')):
            if unit.properties['name'].endswith(('Quantity',
                                                 'QuantityArray')):
                instant_spec = {
                    'T': (
                        cls.template_spec['scalar']
                        + cls.template_spec['json']
                    )
                }
            spec = {
                'Encoding': EncodingType,
            }
        elif unit.properties['name'].endswith(('Array', 'Object',
                                               'MemberIterator')):
            spec = {'Const': ['true', 'false']}
            if unit.properties['name'].endswith('MemberIterator'):
                spec['Encoding'] = EncodingType
                spec['Allocator'] = AllocatorType
            else:
                spec['ValueT'] = ValueType
                kws['add_members_specialized'] = {
                    ('true', ValueType): {
                        'members': [
                            CXXTypedefUnit.parse(
                                'typedef const Value ValueType;')],
                        'index': 0
                    },
                    ('false', ValueType): {
                        'members': [
                            CXXTypedefUnit.parse(
                                'typedef Value ValueType;')],
                        'index': 0
                    },
                }
                if unit.properties['name'].endswith('Object'):
                    kws['remove_members_specialized'] = {
                        ('true', ValueType): {
                            'member_names': [
                                'AddMember', 'EraseMember',
                                'RemoveMember', 'RemoveAllMembers',
                            ]
                        }
                    }
                else:
                    kws['remove_members_specialized'] = {
                        ('true', ValueType): {
                            'member_names': [
                                'PushBack', 'Erase', 'Clear'
                            ]
                        }
                    }
            new_name = {}
            new_name[tuple(['false'] + list(spec.values())[1:])] = (
                name_base)
            new_name[tuple(['true'] + list(spec.values())[1:])] = (
                f'Const{name_base}')
        elif unit.properties['name'].endswith(('Value', 'Document',
                                               'Member')):
            spec = {
                'Encoding': EncodingType,
                'Allocator': AllocatorType,
            }
            if unit.properties['name'].endswith('Document'):
                spec['StackAllocator'] = StackAllocatorType
            kws['specialize_method_args'] = True
        elif unit.properties['name'].endswith('StringRef'):
            spec = {
                'CharType': CXXTypeUnit.parse('char'),
            }
        else:
            msg = (f"Specialization of {unit.properties['name']} "
                   f"not supported")
            print(msg)
            pprint.pprint(unit.properties)
            raise NotImplementedError(msg)
        unit.parent_unit.specialize(unit.properties['name'], spec,
                                    new_name=new_name, **kws)
        if instant_spec:
            unit.properties['instant_spec'] = unit.specialized_types(
                instant_spec)

    @property
    def final_modified_members(self):
        out = {}
        out[('rapidjson', None, None)] = [
            self._wrap_value_returns,
            # self._specialize_method_template,
        ]
        if getattr(self, 'wrap_rapidjson', False):
            out[('rapidjson', None, None)].append(
                self._wrap_returned_references)
        out[('rapidjson', None)] = [self._specialize_class_template]
        return out

    @property
    def added_properties(self):
        out = super(RapidjsonDocumentFile, self).added_properties
        out[('GenericDocument', )] = {
            'base_class_unit': self._get_unit(('GenericValue', )),
            'type_constructors': [
                'int', 'unsigned', 'int64_t', 'uint64_t',
                'std::string', 'float', 'double',
            ],
        }
        return out


class RapidjsonEnumFile(RapidjsonInterfaceFileBase):

    name = 'rapidjson_enum'
    filename = os.path.join('rapidjson', 'rapidjson.h')
    _provided_types = ['Type', 'YggSubType', 'YggEncodingType']
    _selected_members = {
        None: {
            'member_units': ['enum'],
        },
    }
    _removed_members = {
        None: {
            'member_names': [
                # TODO: Added support
                'Type',
            ],
        }
    }


class RapidjsonUnitsFile(RapidjsonInterfaceFileBase):

    # disabled = True
    name = 'rapidjson_units'
    filename = os.path.join('rapidjson', 'units.h')
    _provided_types = [
        'Quantity', 'QuantityArray', 'UnitsType',
        'GenericUnits', 'GenericQuantity', 'GenericQuantityArray',
    ]
    _selected_members = {
        ('units', ): {
            'member_names': [
                'GenericUnits', 'GenericQuantity', 'GenericQuantityArray',
            ],
        }
    }
    _removed_members = {
        ('units', None): {
            'member_names': [
                # Requires wrapping templated type
                'transcode',
            ],
        }
    }

    @property
    def final_modified_members(self):
        out = {}
        out[('rapidjson', 'units', None)] = [
            RapidjsonDocumentFile._specialize_class_template]
        return out


class RapidjsonObjFile(RapidjsonInterfaceFileBase):

    disabled = True
    name = 'rapidjson_obj'
    filename = os.path.join('rapidjson', 'obj.h')
    _provided_types = ['ObjWavefront']
    _removed_properties = {
        ('ObjWavefront', ): [
            'base',
        ]
    }
    _selected_members = {
        None: {
            'member_names': ['ObjWavefront'],
        }
    }


class RapidjsonPlyFile(RapidjsonInterfaceFileBase):

    disabled = True
    name = 'rapidjson_ply'
    filename = os.path.join('rapidjson', 'ply.h')
    _provided_types = ['Ply']
    _removed_properties = {
        ('Ply', ): [
            'base',
        ]
    }
    _selected_members = {
        None: {
            'member_names': ['Ply'],
        }
    }


class RapidjsonSchemaFile(RapidjsonInterfaceFileBase):

    disabled = True
    name = 'rapidjson_schema'
    filename = os.path.join('rapidjson', 'schema.h')
    _provided_types = [
        'GenericSchemaDocument', 'GenericSchemaValidator',
        'GenericSchemaNormalizer', 'GenericSchemaEncoder',
    ]
    _removed_properties = {}
    _selected_members = {
        None: {
            'member_names': [
                'GenericSchemaDocument', 'GenericSchemaValidator',
                'GenericSchemaNormalizer', 'GenericSchemaEncoder',
            ],
        }
    }


class CommBaseFile(InterfaceFileBase):

    name = 'commbase'
    filename = os.path.join(
        'cpp', 'include', 'communicators', 'CommBase.hpp')
    depends_on = ['enum', 'rapidjson_document']
    _provided_types = ['Comm_t']
    _selected_members = {
        ('YggInterface', 'communicator'): {
            'member_names': ['Comm_t'],
        },
    }
    _removed_members = {
        ('YggInterface', 'communicator', 'Comm_t'): {
            'member_names': [
                'callRealloc', 'recvRealloc',
                'vRecv', 'vSend', 'vCall', 'send_array', 'recv_array',
                'send_dict', 'recv_dict',
                # TODO: These require wrapping templated type
                'setFilters', 'setTransforms',
            ],
            'member_types': [
                'utils::Header', 'utils::Metadata', 'utils::Address',
                'Header', 'Metadata', 'WorkerList',
                'YggInterface::utils::Metadata',
                'YggInterface::utils::Header', 'char*', 'const char*',
                'char',
            ]
        },
    }
    _removed_properties = {
        ('YggInterface', 'communicator', 'Comm_t'): [
            'base_class',
        ]
    }

    def _mark_return_types(self, unit):
        for x in unit.properties['args']:
            if x.properties['name'] == 'data':
                x.properties['is_output'] = True

    @property
    def modified_members(self):
        out = {}
        out[('YggInterface', 'communicator', 'Comm_t', 'recv')] = (
            self._mark_return_types)
        return out

    @property
    def added_members(self):
        out = {}
        out[('YggInterface', 'communicator')] = CXXFunctionUnit(
            name='ygg_cleanup', args=[], type=CXXTypeUnit.parse('void'),
            call_at_exit=True, preprocess_contexts=[])
        return out


class WrapCommFile(InterfaceFileBase):

    name = 'wrapcomm'
    filename = os.path.join(
        'cpp', 'include', 'communicators', 'WrapComm.hpp')
    depends_on = ['commbase']
    _provided_types = ['WrapComm']
    _related_files = ['commbase']
    _selected_members = {
        ('YggInterface', 'communicator', 'WrapComm'): {
            'member_units': ['constructor']
        },
    }
    _removed_members = {
        ('YggInterface', 'communicator', 'WrapComm'): {
            'member_types': [
                'utils::Header', 'utils::Metadata', 'utils::Address',
                'WrapComm&&',
            ]
        },
    }

    def _strip_constructor_args(self, unit):
        if unit.unit_type == 'constructor':
            unit.properties['args'] = [
                x for x in unit.properties['args']
                if (x.properties['type'].properties['base'].properties['name']
                    != 'SupplementCommArgs')]

    @property
    def modified_members(self):
        out = {}
        out[('YggInterface', 'communicator', 'WrapComm', None)] = (
            self._strip_constructor_args)
        return out

    @property
    def added_properties(self):
        out = super(WrapCommFile, self).added_properties
        assert self.related_files.get('commbase', None)
        out[('YggInterface', 'communicator', 'WrapComm')] = {
            'base_class_unit': self.related_files['commbase']._get_unit(
                ('YggInterface', 'communicator', 'Comm_t')),
        }
        return out


class EnumFile(InterfaceFileBase):

    name = 'enum'
    _provided_types = [
        'COMM_TYPE', 'DIRECTION',  # TODO: more...
    ]
    filename = os.path.join('cpp', 'include', 'utils', 'enums.hpp')
    _selected_members = {
        None: {
            'member_units': ['enum']
        },
    }


class CollectedInterfaceFileBase(InterfaceFileBase):

    _modules = {}

    @classmethod
    def load(cls, **kwargs):
        kwargs.update(
            dont_read=True,
            dont_register=True,
        )
        return super(CollectedInterfaceFileBase, cls).load(**kwargs)

    def _assemble(self, unit):
        for k, v in self._modules.items():
            kws = dict(v.get('properties', {}), external_types=[])
            base_unit = self.related_files[v['base_unit']]._get_unit(
                v['base_unit_module'])
            for src in v.get('types_from', []):
                y = self._modules[src]
                for x in [y['base_unit']] + y['draw_from']:
                    kws['external_types'] += [
                        xx for xx in
                        self.related_files[x].provided_types
                        if xx not in kws['external_types']
                    ]
            unit.add_member(CXXModuleUnit(k, base_unit=base_unit, **kws))
            for x in v['draw_from']:
                if self.related_files[x].disabled:
                    continue
                src = self.related_files[x]
                if x in v.get('draw_from_module', {}):
                    src = src._get_unit(v['draw_from_module'][x])
                else:
                    src = src._get_unit(v['base_unit_module'])
                unit[k].copy_members(src)
        unit.set_property('indent', unit.properties['indent'])
        print(f"ASSEMBLED: {pprint.pformat(unit.properties)}")

    def make_modifications(self):
        self._assemble(self.unit)
        super(CollectedInterfaceFileBase, self).make_modifications()


class YggInterfaceFile(CollectedInterfaceFileBase):

    filename = os.path.join('cpp', 'include', 'YggInterface.hpp')
    _related_files = [
        'rapidjson_document',
        'rapidjson_enum',
        'rapidjson_units',
        'rapidjson_obj', 'rapidjson_ply',
        'commbase', 'wrapcomm', 'enum',
    ]
    _modules = {
        'rapidjson': {
            'base_unit': 'rapidjson_document',
            'base_unit_module': ('rapidjson', ),
            'draw_from': [
                'rapidjson_enum', 'rapidjson_obj', 'rapidjson_ply',
                'rapidjson_units', 'rapidjson_document',
            ],
        },
        'YggInterface': {
            'base_unit': 'commbase',
            'base_unit_module': ('YggInterface', 'communicator'),
            'draw_from': [
                'enum', 'commbase', 'wrapcomm',
            ],
            'draw_from_module': {
                'enum': None,
            },
            'types_from': ['rapidjson'],
        },
    }


class RapidjsonInterfaceFile(RapidjsonInterfaceFileMixin,
                             CollectedInterfaceFileBase):

    filename = os.path.join('rapidjson', 'document.h')
    _related_files = [
        'rapidjson_document',
        'rapidjson_units',
        'rapidjson_obj', 'rapidjson_ply',
    ]
    _modules = {
        'rapidjson': {
            'base_unit': 'rapidjson_document',
            'base_unit_module': ('rapidjson', ),
            'draw_from': [
                'rapidjson_obj', 'rapidjson_ply',
                'rapidjson_units', 'rapidjson_document',
            ],
        },
    }


class Interface(GeneratedFile, metaclass=GeneratedInterfaceFileMeta):

    name = None
    modifications = {}

    def __init__(self, *args, **kwargs):
        for k in ['wrap_rapidjson', 'rapidjson_include_dirs']:
            setattr(self, k, kwargs.pop(k, None))
        super(Interface, self).__init__(*args, **kwargs)

    def generate(self, *args, **kwargs):
        verbose = kwargs.get('verbose', False)
        wrap_kwargs = kwargs.pop('wrap_kwargs', {})
        base_kwargs = kwargs.pop('base_kwargs', {})
        for k in ['wrap_rapidjson', 'rapidjson_include_dirs']:
            if getattr(self, k) is not None:
                base_kwargs[k] = getattr(self, k)
        base_kwargs['modifications'] = copy.deepcopy(
            self.modifications)
        base_kwargs['modification_functions'] = {}
        fygg_cls = kwargs.pop('interface_file_class', YggInterfaceFile)
        for k in fygg_cls.modification_types:
            if hasattr(self, k):
                base_kwargs['modification_functions'][k] = getattr(self, k)
        fygg = fygg_cls(verbose=verbose, **base_kwargs)
        base = self.modify_base(fygg.unit)
        self.wrap_unit(base, **wrap_kwargs)
        print("GENERATING")
        return super(Interface, self).generate(*args, **kwargs)

    def modify_base(self, base):
        out = copy.deepcopy(base)
        out.base_unit = base
        return out


class RJWrapperInterface(Interface):

    name = 'rjwrapper'

    def __init__(self, **kwargs):
        header = os.path.join(
            'cpp', 'include', 'utils', 'rapidjson_wrapper2.hpp')
        src = os.path.join(
            'cpp', 'src', 'utils', 'rapidjson_wrapper2.cpp')
        added = {
            'source': GeneratedFile(src, language='rjwrapper_source')}
        super(RJWrapperInterface, self).__init__(
            header, language='rjwrapper', added=added, **kwargs)

    def generate(self, *args, **kwargs):
        kwargs.setdefault('interface_file_class', RapidjsonInterfaceFile)
        return super(RJWrapperInterface, self).generate(*args, **kwargs)

    def from_unit(self, *args, **kwargs):
        out = super(RJWrapperInterface, self).from_unit(*args, **kwargs)
        new_members = [
            CMacroUnit.parse(
                '#define RAPIDJSON_DEFAULT_ALLOCATOR ::RAPIDJSON'
                '_NAMESPACE::MemoryPoolAllocator< ::RAPIDJSON_NA'
                'MESPACE::CrtAllocator >'),
            CMacroUnit.parse(
                '#define RAPIDJSON_DEFAULT_STACK_ALLOCATOR ::RAP'
                'IDJSON_NAMESPACE::CrtAllocator'),
        ]
        out.properties['defines'] = new_members + out.properties.get(
            'defines', [])
        return out

    def modify_base(self, base):
        out = super(RJWrapperInterface, self).modify_base(base)
        out.properties.setdefault('includes', [])
        local_includes = [
            "rapidjson/rapidjson.h",
            "rapidjson/allocators.h",
            "rapidjson/units.h",
            "rapidjson/ply.h",
            "rapidjson/obj.h",
        ]
        for k in local_includes:
            out.properties['includes'].append(
                CImportUnit(name=k, local_ctx=True))
        for unit in out['rapidjson'].properties['members']:
            new_members = []
            if unit.properties['name'].endswith(('Array', 'Object')):
                new_members += [
                    CXXTypedefUnit.parse('typedef Value ValueT;'),
                ]
            if unit.properties['name'].endswith(('Array', 'Object')):
                template_idx = [
                    i for i, x in enumerate(
                        unit.properties['template'].properties['param'])
                    if x.properties['name'] == 'Const'
                ]
                template_param = [
                    unit.properties['template'].properties['param'][i]
                    for i in template_idx
                ]
                unit.properties['template'].properties['param'] = (
                    template_param)
                for x in unit.properties['members']:
                    if not (x.unit_type == 'typedef'
                            and (x.properties['type'].properties[
                                'base'].properties['name']
                                 == unit.properties['name'])):
                        continue
                    x.properties['type'].properties['template_spec'] = [
                        x.properties['type'].properties['template_spec'][i]
                        for i in template_idx
                    ]
            assert None not in new_members
            unit.properties['members'] = (
                new_members + unit.properties['members'])
        return out
