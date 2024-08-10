import os
import pprint
from generate_generic import GeneratedFile
from generate_generic.base import get_file_unit


def get_interface_baseunit(verbose=False):
    fygg = get_file_unit(
        os.path.join('cpp', 'include', 'YggInterface.hpp'),
        verbose=verbose)
    fcpp = get_file_unit(
        os.path.join('cpp', 'include', 'communicators', 'CommBase.hpp'),
        verbose=verbose)
    for x in fygg.properties['members']:
        if x.unit_type == 'class':
            x.copy_members(
                fcpp['YggInterface']['Comm_t'], member_units=['method'])
    fcpp['YggInterface']['CommBase'].copy_members(
        fcpp['YggInterface']['Comm_t'])
    fcpp['YggInterface'].set_property(
        'members', [fcpp['YggInterface']['CommBase']])
    pprint.pprint(fcpp['YggInterface'].properties)
    pprint.pprint(fcpp['YggInterface']['CommBase'].properties)
    return fcpp


class Interface(GeneratedFile):

    def generate(self, *args, **kwargs):
        verbose = kwargs.get('verbose', False)
        base = get_interface_baseunit(verbose=verbose)
        self.wrap_unit(base)
        return super(Interface, self).generate(*args, **kwargs)


class JuliaInterface(Interface):

    # TODO:
    # - Fix indentation
    # - Change to wrapped comm
    # - Change method to using WrapComm?

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
