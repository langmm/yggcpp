#pragma once
#include <string>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <vector>
#include <atomic>
#include <regex>

//#ifdef __cplusplus
//extern "C"
//{
//#endif

#ifdef ELF_AVAILABLE
#include <elf.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

int get_module_base_address(char const *module_filename, void *handle, void **base);
void *elf_hook(char const *library_filename, void const *library_address, const std::string &name, void const *substitution_address);
#endif // ELF_AVAILABLE

//#ifdef __cplusplus
//}
//#endif
