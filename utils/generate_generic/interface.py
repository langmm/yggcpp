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
    fwrp['YggInterface']['communicator']['WrapComm'].select_members(
        member_units=['constructor'])
    fwrp['YggInterface']['communicator']['WrapComm'].copy_members(
        fcpp['YggInterface']['communicator']['Comm_t'],
        member_units=['method'])
    fwrp['YggInterface']['communicator'].set_property(
        'members', [fwrp['YggInterface']['communicator']['WrapComm']])
    members = []
    for x in fwrp['YggInterface']['communicator'][
            'WrapComm'].properties.get('members'):
        args = x.format_property('args', x.properties['args'])
        if (((x.properties.get('name', None) not in ['send', 'recv', 'call']
              or 'rapidjson::Document' in args)
             and 'utils::Header' not in args)):
            members.append(x)
    fwrp['YggInterface']['communicator']['WrapComm'].set_property(
        'members', members)
    fwrp['YggInterface']['communicator']['WrapComm'].remove_members(
        member_names=['callRealloc', 'recvRealloc',
                      'vRecv', 'vSend', 'vCall'])
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
        return base


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
        base = copy.deepcopy(super(JuliaInterface, self).modify_base(base))
        base.set_property(
            'members',
            base['YggInterface'].properties['members'])
        return base

    def from_unit(self, x, **kwargs):
        out = super(JuliaInterface, self).from_unit(x, **kwargs)
        out['communicator']['WrapComm'].set_property('name', 'YggComm')
        return out
