import os
import copy
import pprint
from contextlib import contextmanager
from generate_generic import GeneratedFile
from generate_generic.base import get_file_unit
from generate_generic.cpp import (
    CXXModuleUnit, CXXFunctionUnit, CXXTypeUnit)


# TODO:
# - Add field unit?
# - Add property unit with setter/getter
# - Add ObjWavefront, Ply, GenericQuantity, GenericQuantityArray
# - Handle separation of C/CXX header/source
# - Get docs by looking backwards for comments
# PYTHONPATH=utils python -m generate_generic.runner --language=julia
#   --wrap-rapidjson --verbose --debug
# python utils/generate_generic.py --language=julia


_interface_file_registry = {
    'classes': {},
    'instances': {},
}


def register_interface_file(k, v, instance=False):
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
    _selected_members = {}
    _removed_members = {}
    _modified_members = {}
    _added_members = {}
    _added_properties = {}
    _removed_properties = {}
    _added_containers = {}
    _replaced_types = {}

    def __init__(self, related_files=None, **kwargs):
        self.unit = self.load(**kwargs)
        self.name_changes = {}
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
        for k, v in self.selected_members.items():
            for x in self._get_units(k):
                x.select_members(**v)
        for k, v in self.removed_members.items():
            for x in self._get_units(k):
                x.remove_members(**v)
        for k, v in self.modified_members.items():
            for x in self._get_units(k):
                v(x)
        for k, v in self.added_members.items():
            for x in self._get_units(k):
                x.add_member(v, make_copy=True)
        for k, v in self.added_properties.items():
            for x in self._get_units(k):
                for kk, vv in v.items():
                    x.set_property(kk, vv)
        for k, v in self.removed_properties.items():
            for x in self._get_units(k):
                for vv in v:
                    x.properties.pop(vv, None)
        for k, v in self.added_containers.items():
            for x in self._get_units(k):
                x.add_container(v[0], **v[1])
        for k, v in self.replaced_types.items():
            for x in self._get_units(k):
                for kk, vv in v.items():
                    x.replace_type(kk, vv)

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
                            tmp += x.properties['members']
                        out = tmp
                    else:
                        out = out.properties['members']
                else:
                    assert isinstance(k, str)
                    if isinstance(out, list):
                        out = [x[k] for x in out]
                    else:
                        out = out[k]
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

    @property
    def selected_members(self):
        return copy.deepcopy(self._selected_members)

    @property
    def provided_types(self):
        return copy.deepcopy(self._provided_types)

    @property
    def removed_members(self):
        out = copy.deepcopy(self._removed_members)
        for x in self.depends_on:
            if self.related_files[x].disabled:
                out.setdefault(True, {'member_types': []})
                out[True]['member_types'] += self.related_files[
                    x].provided_types
        return out

    @property
    def modified_members(self):
        return copy.deepcopy(self._modified_members)

    @property
    def added_members(self):
        return copy.deepcopy(self._added_members)

    @property
    def added_properties(self):
        out = copy.deepcopy(self._added_properties)
        out.setdefault(None, {})
        out[None]['ignored_preprocess_contexts'] = (
            self.ignored_preprocess_contexts)
        out.setdefault(True, {})
        out[True]['file_unit'] = self.unit.get_property('file_unit')
        return out

    @property
    def removed_properties(self):
        out = copy.deepcopy(self._removed_properties)
        return out

    @property
    def added_containers(self):
        return copy.deepcopy(self._added_containers)

    @property
    def replaced_types(self):
        return copy.deepcopy(self._replaced_types)

    @classmethod
    def load(cls, filename=None, **kwargs):
        if cls.disabled:
            kwargs['dont_read'] = True
        if filename is None:
            filename = cls.filename
        unit = get_file_unit(filename, language=cls.language,
                             disable_api=cls.disable_api, **kwargs)
        return unit


class RapidjsonInterfaceFileBase(InterfaceFileBase):

    language = 'cxx'
    disable_api = True
    _replaced_types = {
        None: {
            'Ch': 'char',
            'SizeType': 'unsigned',
            'ValueType': 'Value',
            'GenericValue': 'Value',
            'GenericDocument': 'Document',
        },
    }
    _added_containers = {
        None: ('module', {'name': 'rapidjson'}),
    }

    def __init__(self, wrap_rapidjson=False, **kwargs):
        self.wrap_rapidjson = wrap_rapidjson
        super(RapidjsonInterfaceFileBase, self).__init__(**kwargs)

    @classmethod
    def load(cls, filename=None, rapidjson_include_dirs=None,
             **kwargs):
        if filename is None:
            filename = cls.filename
        if rapidjson_include_dirs is None:
            rapidjson_include_dirs = os.path.join(
                'cpp', 'include', 'rapidjson', 'include')
        kwargs['filename'] = os.path.join(
            rapidjson_include_dirs, filename)
        unit = super(RapidjsonInterfaceFileBase, cls).load(**kwargs)
        return unit

    @property
    def removed_members(self):
        out = super(RapidjsonInterfaceFileBase, self).removed_members
        for x in self.unit.properties['members']:
            if x.unit_type != 'class':
                continue
            for m in x.properties.get('members', []):
                if 'template' in m.properties:
                    self.append_removed_member(out, m)
        return out


class RapidjsonDocumentFile(RapidjsonInterfaceFileBase):

    name = 'rapidjson_document'
    filename = os.path.join('rapidjson', 'document.h')
    depends_on = [
        'rapidjson_enum', 'rapidjson_ply', 'rapidjson_obj',
        'rapidjson_units',
    ]
    _provided_types = ['Value', 'Document']
    _selected_members = {
        None: {
            'member_names': ['GenericValue', 'GenericDocument'],
        }
    }
    _removed_members = {
        (None, ): {
            'member_names': [
                'GET_SCALAR_METHOD', '_data', 'v', 'free', 'data_',
                'BASE_STD_SCALAR_', 'data', 'RAPIDJSON_ASSERT',
                'shape_array', 'DoReserveMembers', 'StackPop',
                'StackTop', 'StackBottom', 'StackSize', 'ClearStack',
                'CountVarArgs', 'SkipVarArgs', 'SetVarArgs',
                'SetVarArgsRealloc', 'GetVarArgs',
                'ApplyVarArgs',
            ],
            'member_types': [
                'PyObject', 'Allocator', 'Array', 'ConstArray',
                'Object', 'ConstObject', 'SchemaValueType',
                'MemberIterator', 'ConstMemberIterator',
                'ValueIterator', 'ConstValueIterator',
                'VarArgList', 'StringRefType', 'StringBuffer',
                'GenericStringBuffer',
                # TODO: Added support
                'Type',
            ],
        }
    }

    @property
    def removed_members(self):
        out = super(RapidjsonDocumentFile, self).removed_members
        for x in self.unit.properties['members']:
            for m in x.properties['members']:
                if ((m.unit_type == 'constructor'
                     and len(m.properties['args']) == 1)):
                    m_type = m.properties['args'][0].properties['type']
                    if m_type.properties.get('ptr', '').strip() == '&&':
                        self.append_removed_member(out, m)
        return out

    def _wrap_value_returns(self, unit):
        if not (unit.unit_type == 'method'
                and (unit.properties['type'].properties['base'] in
                     ['GenericValue', 'ValueType', 'Value',
                      'GenericDocument', 'Document'])):
            return
        name = unit.properties['name']
        m_type = unit.properties['type']
        if name in ['GetTitle', 'GetUnits', 'YggSubTypeString',
                    'GetYggType', 'GetSubType', 'GetEncoding']:
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'std::basic_string<Ch>')
        elif name in ['GetShape']:
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'std::vector<SizeType>')
        elif (name in ['Swap', 'PopBack', 'AddMember', 'Parse']
              or name.startswith('Set')):
            unit.properties['wrapped_type'] = CXXTypeUnit.parse(
                'void')
        else:
            print(name)
            import pdb
            pdb.set_trace()
        if ((self.wrap_rapidjson
             and '&' in m_type.properties.get('ptr', ''))):
            m_type.properties['ptr'] = m_type.properties[
                'ptr'].replace('&', '')

    def _specialize_template(self, unit):
        if self.wrap_rapidjson:
            for x in ['template', 'template_spec']:
                unit.properties.pop(x, None)
        else:
            param = [
                CXXTypeUnit.parse('UTF8<>'),
                CXXTypeUnit.parse('RAPIDJSON_DEFAULT_ALLOCATOR'),
            ]
            if unit.properties['name'].endswith('Document'):
                param.append(
                    CXXTypeUnit.parse(
                        'RAPIDJSON_DEFAULT_STACK_ALLOCATOR'))
            unit.parent_unit.specialize(
                unit.properties['name'], param)

    @property
    def modified_members(self):
        out = super(RapidjsonDocumentFile, self).modified_members
        out[(None, None)] = self._wrap_value_returns
        out[(None, )] = self._specialize_template
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

    # disabled = True
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

    disabled = True
    name = 'rapidjson_units'
    filename = os.path.join('rapidjson', 'units.h')
    _provided_types = [
        'Quantity', 'QuantityArray', 'UnitsType',
        'GenericQuantity', 'GenericQuantityArray',
    ]
    _selected_members = {
        None: {
            'member_names': [
                'GenericQuantity', 'GenericQuantityArray', 'UnitsType',
            ],
        }
    }


class RapidjsonObjFile(RapidjsonInterfaceFileBase):

    disabled = True
    name = 'rapidjson_obj'
    filename = os.path.join('rapidjson', 'obj.h')
    _provided_types = ['ObjWavefront']
    _removed_properties = {
        ('ObjWavefront', ): [
            'base_class',
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
            'base_class',
        ]
    }
    _selected_members = {
        None: {
            'member_names': ['Ply'],
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
                # 'send', 'recv', 'call',
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
        out = super(CommBaseFile, self).modified_members
        out[('YggInterface', 'communicator', 'Comm_t', 'recv')] = (
            self._mark_return_types)
        return out

    @property
    def added_members(self):
        out = super(CommBaseFile, self).added_members
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
                if (x.properties['type'].properties['base']
                    != 'SupplementCommArgs')]

    @property
    def modified_members(self):
        out = super(WrapCommFile, self).modified_members
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


class YggInterfaceFile(InterfaceFileBase):

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

    @classmethod
    def load(cls, **kwargs):
        kwargs['dont_read'] = True
        return super(YggInterfaceFile, cls).load(**kwargs)

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
        pprint.pprint(unit.properties)

    @property
    def modified_members(self):
        out = super(YggInterfaceFile, self).modified_members
        out[None] = self._assemble
        return out


class Interface(GeneratedFile):

    def generate(self, *args, **kwargs):
        verbose = kwargs.get('verbose', False)
        kws_base = {
            k: kwargs.pop(k) for k in
            ['wrap_rapidjson', 'rapidjson_include_dirs']
            if k in kwargs
        }
        fygg = YggInterfaceFile(verbose=verbose, **kws_base)
        base = self.modify_base(fygg.unit)
        self.wrap_unit(base)
        return super(Interface, self).generate(*args, **kwargs)

    def modify_base(self, base):
        out = copy.deepcopy(base)
        out.base_unit = base
        return out


class JuliaInterface(Interface):

    def __init__(self):
        src_jl = (os.path.join('julia', 'YggInterface',
                               'src', 'YggInterface.jl'),
                  'julia')
        src_cp = (os.path.join('julia', 'YggInterface_julia.cpp'),
                  'julia_cxxwrap')
        added = {
            'julia_cxxwrap': GeneratedFile(
                src_jl[0], language=src_jl[1]
            )
        }
        super(JuliaInterface, self).__init__(
            src_cp[0], language=src_cp[1], added=added)

    def modify_base(self, base):
        out = super(JuliaInterface, self).modify_base(base)
        for cls in out['rapidjson'].properties['members']:
            for m in cls.properties['members']:
                if m.unit_type != 'method':
                    continue
                if m.properties['name'] == 'SetString':
                    m.properties['args'] = [m.properties['args'][0]]
                    x = m.properties['args'][0]
                    x.properties['type'].properties.update(
                        base='std::string')
                    x.properties['type'].properties['ptr'] = '&'
                    m.set_property(
                        'wrapper_body',
                        f"self.{m.properties['name']}("
                        f"{x.properties['name']}.c_str(), "
                        f"{x.properties['name']}.size());"
                    )
                elif ((m.properties['type'].properties['base'] == 'char'
                       and m.properties['name'] not in ['GetUnits'])):
                    m.set_property('wrapped_type',
                                   CXXTypeUnit.parse('std::string'))
                if ((m.unit_type != 'method'
                     or 'wrapped_type' not in m.properties
                     or 'wrapper_body' in m.properties)):
                    continue
                wrapT = m.properties['wrapped_type']
                args = m.properties['args']
                const = m.get_property('const_method', '')
                static = m.get_property('static', '')
                m_type0 = m.properties['type']
                if const:
                    const = const.strip() + ' '
                m.set_property('type', wrapT)
                wrapT_str = wrapT.format()
                if static:
                    src = f"{m.get_property('parent')}::"
                else:
                    src = 'self.'
                if m_type0.properties['base'] in ['char', 'Ch']:
                    m.set_property(
                        'wrapper_body',
                        f"return {wrapT_str}({src}{m.properties['name']}("
                        f"{', '.join(x.properties['name'] for x in args)}"
                        f"));")
                elif wrapT_str == 'void':
                    m.set_property(
                        'wrapper_body',
                        f"{src}{m.properties['name']}("
                        f"{', '.join(x.properties['name'] for x in args)}"
                        f");")
                else:
                    m.set_property(
                        'wrapper_body',
                        f"return {src}{m.properties['name']}("
                        f"{', '.join(x.properties['name'] for x in args)}"
                        f").Get<{wrapT_str}>();")
            members = []
            for m in cls.properties['members']:
                if m.unit_type != 'method':
                    members.append(m)
                    continue
                if ((m.properties['name'] in ['String', 'Int', 'Uint',
                                              'Int64', 'Uint64',
                                              'Float', 'Double']
                     or m.properties['type'].properties['base'] == 'char'
                     or ('const char*' in m.utilized_types()
                         and m.properties['name'] in ['SetUnits']))):
                    continue
                members.append(m)
            cls.properties['members'] = members
        return out

    def from_unit(self, x, **kwargs):
        out = super(JuliaInterface, self).from_unit(x, **kwargs)
        out['YggInterface']['WrapComm'].set_property(
            'name', 'YggComm')
        return out
