import os
import copy
import pprint
from generate_generic import GeneratedFile
from generate_generic.base import get_file_unit


def get_interface_baseunit(verbose=False):
    fwrp = get_file_unit(
        os.path.join('cpp', 'include', 'communicators', 'WrapComm.hpp'),
        verbose=verbose)
    fcpp = get_file_unit(
        os.path.join('cpp', 'include', 'communicators', 'CommBase.hpp'),
        verbose=verbose)
    members = []
    for x in fcpp['YggInterface']['communicator'][
            'Comm_t'].properties.get('members'):
        if 'args' not in x.properties:
            continue
        # TODO: Handle return type separately?
        args = x.format_property('args', x.properties['args'])
        args += x.format_property('type', x.properties['type'])
        if (((x.properties.get('name', None) not in ['send', 'recv', 'call']
              or 'rapidjson::Document' in args)
             and 'utils::Header' not in args
             and 'utils::Metadata' not in args
             and not x.properties.get('static', ''))):
            members.append(x)
    fcpp['YggInterface']['communicator']['Comm_t'].set_property(
        'members', members)
    fcpp['YggInterface']['communicator']['Comm_t'].remove_members(
        member_names=['callRealloc', 'recvRealloc',
                      'vRecv', 'vSend', 'vCall'])
    fwrp['YggInterface']['communicator']['WrapComm'].select_members(
        member_units=['constructor'])
    fwrp['YggInterface']['communicator']['WrapComm'].base_unit = (
        fcpp['YggInterface']['communicator']['Comm_t'])
    # fwrp['YggInterface']['communicator']['WrapComm'].copy_members(
    #     fcpp['YggInterface']['communicator']['Comm_t'],
    #     member_units=['method'])
    fwrp['YggInterface']['communicator'].set_property(
        'members', [fwrp['YggInterface']['communicator']['WrapComm']])
    pprint.pprint(fcpp['YggInterface']['communicator']['Comm_t'].properties)
    pprint.pprint(fwrp['YggInterface']['communicator'].properties)
    pprint.pprint(fwrp['YggInterface']['communicator']['WrapComm'].properties)
    return fwrp


class Interface(GeneratedFile):

    def generate(self, *args, **kwargs):
        verbose = kwargs.get('verbose', False)
        base = self.modify_base(get_interface_baseunit(verbose=verbose))
        self.wrap_unit(base)
        return super(Interface, self).generate(*args, **kwargs)

    def modify_base(self, base):
        return copy.deepcopy(base)


class JuliaInterface(Interface):

    def __init__(self):
        src_jl = (os.path.join('julia', 'YggInterface.jl'), 'julia')
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
        base = super(JuliaInterface, self).modify_base(base)
        base.set_property(
            'members',
            base['YggInterface'].properties['members'])
        base['communicator'].add_member(
            base['communicator']['WrapComm'].base_unit)
        members = [
            base['communicator']['Comm_t'],
            base['communicator']['WrapComm'],
        ]
        base['communicator'].properties['members'] = members
        base['communicator']['Comm_t'].properties['base_class'] = ''
        base['communicator']['WrapComm'].properties['base_class'] = 'Comm_t'
        base.set_property('indent', base.properties['indent'])
        return base

    def from_unit(self, x, **kwargs):
        out = super(JuliaInterface, self).from_unit(x, **kwargs)
        out['communicator']['WrapComm'].set_property('name', 'YggComm')
        return out
