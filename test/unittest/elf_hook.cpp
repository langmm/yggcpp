#include <cstdlib>
#include <cstring>
#include <dlfcn.h>
#include <elf.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <cerrno>
#include <vector>
#include <atomic>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <regex>
#include "elf_hook.hpp"
//rename standart types for convenience
#if defined __x86_64 || defined __aarch64__
    #define Elf_Ehdr Elf64_Ehdr
    #define Elf_Shdr Elf64_Shdr
    #define Elf_Sym Elf64_Sym
    #define Elf_Rel Elf64_Rela
    #define ELF_R_SYM ELF64_R_SYM
    #define REL_DYN ".rela.dyn"
    #define REL_PLT ".rela.plt"
#else
    #define Elf_Ehdr Elf32_Ehdr
    #define Elf_Shdr Elf32_Shdr
    #define Elf_Sym Elf32_Sym
    #define Elf_Rel Elf32_Rel
    #define ELF_R_SYM ELF32_R_SYM
    #define REL_DYN ".rel.dyn"
    #define REL_PLT ".rel.plt"
#endif


//==================================================================================================
static int read_header(int d, Elf_Ehdr **header)
{
    *header = (Elf_Ehdr *)malloc(sizeof(Elf_Ehdr));
    if(NULL == *header)
    {
        return errno;
    }

    if (lseek(d, 0, SEEK_SET) < 0)
    {
        free(*header);

        return errno;
    }

    if (read(d, *header, sizeof(Elf_Ehdr)) <= 0)
    {
        free(*header);

        return errno = EINVAL;
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int read_section_table(int d, Elf_Ehdr const *header, Elf_Shdr **table)
{
    size_t size;

    if (NULL == header)
        return EINVAL;

    size = header->e_shnum * sizeof(Elf_Shdr);
    *table = (Elf_Shdr *)malloc(size);
    if(NULL == *table)
    {
        return errno;
    }

    if (lseek(d, header->e_shoff, SEEK_SET) < 0)
    {
        free(*table);

        return errno;
    }

    if (read(d, *table, size) <= 0)
    {
        free(*table);

        return errno = EINVAL;
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int read_string_table(int d, Elf_Shdr const *section, char const **strings)
{
    if (NULL == section)
        return EINVAL;

    *strings = (char const *)malloc(section->sh_size);
    if(NULL == *strings)
    {
        return errno;
    }

    if (lseek(d, section->sh_offset, SEEK_SET) < 0)
    {
        free((void *)*strings);

        return errno;
    }

    if (read(d, (char *)*strings, section->sh_size) <= 0)
    {
        free((void *)*strings);

        return errno = EINVAL;
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int read_symbol_table(int d, Elf_Shdr const *section, Elf_Sym **table)
{
    if (NULL == section)
        return EINVAL;

    *table = (Elf_Sym *)malloc(section->sh_size);
    if(NULL == *table)
    {
        return errno;
    }

    if (lseek(d, section->sh_offset, SEEK_SET) < 0)
    {
        free(*table);

        return errno;
    }

    if (read(d, *table, section->sh_size) <= 0)
    {
        free(*table);

        return errno = EINVAL;
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int section_by_index(int d, size_t const index, Elf_Shdr **section)
{
    Elf_Ehdr *header = NULL;
    Elf_Shdr *sections = NULL;

    *section = NULL;

    if (
        read_header(d, &header) ||
        read_section_table(d, header, &sections)
        )
        return errno;

    if (index < header->e_shnum)
    {
        *section = (Elf_Shdr *)malloc(sizeof(Elf_Shdr));

        if (NULL == *section)
        {
            free(header);
            free(sections);

            return errno;
        }

        memcpy(*section, sections + index, sizeof(Elf_Shdr));
    }
    else
        return EINVAL;

    free(header);
    free(sections);

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int section_by_type(int d, size_t const section_type, Elf_Shdr **section)
{
    Elf_Ehdr *header = NULL;
    Elf_Shdr *sections = NULL;
    size_t i;

    *section = NULL;

    if (
        read_header(d, &header) ||
        read_section_table(d, header, &sections)
        )
        return errno;

    for (i = 0; i < header->e_shnum; ++i)
        if (section_type == sections[i].sh_type)
        {
            *section = (Elf_Shdr *)malloc(sizeof(Elf_Shdr));

            if (NULL == *section)
            {
                free(header);
                free(sections);

                return errno;
            }

            memcpy(*section, sections + i, sizeof(Elf_Shdr));

            break;
        }

    free(header);
    free(sections);

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int section_by_name(int d, char const *section_name, Elf_Shdr **section)
{
    Elf_Ehdr *header = NULL;
    Elf_Shdr *sections = NULL;
    char const *strings = NULL;
    size_t i;

    *section = NULL;

    if (
        read_header(d, &header) ||
        read_section_table(d, header, &sections) ||
        read_string_table(d, &sections[header->e_shstrndx], &strings)
        )
        return errno;

    for (i = 0; i < header->e_shnum; ++i)
        if (!strcmp(section_name, &strings[sections[i].sh_name]))
        {
            *section = (Elf_Shdr *)malloc(sizeof(Elf_Shdr));

            if (NULL == *section)
            {
                free(header);
                free(sections);
                free((void *)strings);

                return errno;
            }

            memcpy(*section, sections + i, sizeof(Elf_Shdr));

            break;
        }

    free(header);
    free(sections);
    free((void *)strings);

    return 0;
}
//--------------------------------------------------------------------------------------------------
static int symbol_by_name(int d, Elf_Shdr *section, const std::string& name, Elf_Sym **symbol, std::atomic_size_t *index)
{
    Elf_Shdr *strings_section = NULL;
    char const *strings = NULL;
    Elf_Sym *symbols = NULL;
    size_t i, amount;

    *symbol = NULL;
    *index = 0;

    if (
        section_by_index(d, section->sh_link, &strings_section) ||
        read_string_table(d, strings_section, &strings) ||
        read_symbol_table(d, section, &symbols)
        )
        return errno;

    amount = section->sh_size / sizeof(Elf_Sym);
    volatile size_t q;

    int idx = -1;
    std::vector<std::string> args;
    std::string repname = name;
    if (size_t start = repname.find("|"); start != std::string::npos) {
        std::string a = repname.substr(start + 1);
        repname.resize(start);
        boost::split(args, a, boost::is_any_of("|"));
    }
    if (repname.find("::") != std::string::npos) {
        repname = std::regex_replace(repname, std::regex("<(\\S+)>"), "\\D+\\d{1,2}$1\\D{1,2}");
        //std::vector<std::string> symbolNames(amount);
        //std::vector<std::string> parts;
        //boost::algorithm::split_regex(parts, name, token);
        std::string regexstr = "\\S+" + std::regex_replace(repname, std::regex("::"), "\\d{1,2}");
        //for(i = 1; i < parts.size(); i++) {
        //    regexstr += "\\d{1,2}" + parts[i];
        //}
        regexstr += "(?:[A-Z]|\\d)";
        boost::regex regex{regexstr};

        for (i = 0; i < amount; i++) {
            std::string sname = &strings[symbols[i].st_name];
            boost::smatch smatch;
            if (boost::regex_search(sname, smatch, regex)) {
                if (!args.empty()) {
                    std::string match = smatch[0];
                    size_t start = sname.find(match) + match.size();
                    bool ok = true;
                    for (auto s: args) {
                        if (sname.find(s, start) != std::string::npos) {
                            start += s.size();
                            continue;
                        } else {
                            ok = false;
                            break;
                        }
                    }
                    if (!ok)
                        continue;
                }
                const char *temp = &strings[symbols[i].st_name];
                std::string nae(temp);
                Elf64_Word xx = symbols[i].st_name;
                size_t yy = sizeof(&strings);

                idx = i;
                break;
            }
        }
    } else {
        for (i = 0; i < amount; ++i) {
            const char *temp = &strings[symbols[i].st_name];
            std::string nae(temp);
            Elf64_Word xx = symbols[i].st_name;
            size_t yy = sizeof(&strings);
            if (!strcmp(name.c_str(), &strings[symbols[i].st_name])) {
                idx = i;
                break;
            }
        }
    }
    if (idx >= 0) {
        *symbol = (Elf_Sym *) malloc(sizeof(Elf_Sym));

        if (NULL == *symbol) {
            free(strings_section);
            free((void *) strings);
            free(symbols);

            return errno;
        }

        memcpy(*symbol, symbols + idx, sizeof(Elf_Sym));
        *index = idx;
        q = idx;
    }
    free(strings_section);
    free((void *)strings);
    free(symbols);

    return 0;
}
//--------------------------------------------------------------------------------------------------
int get_module_base_address(char const *module_filename, void *handle, void **base)
{
    int descriptor;  //file descriptor of shared module
    Elf_Shdr *dynsym = NULL, *strings_section = NULL;
    char const *strings = NULL;
    Elf_Sym *symbols = NULL;
    size_t i, amount;
    Elf_Sym *found = NULL;

    *base = NULL;

    descriptor = open(module_filename, O_RDONLY);

    if (descriptor < 0)
        return errno;

    if (section_by_type(descriptor, SHT_DYNSYM, &dynsym) ||  //get ".dynsym" section
        section_by_index(descriptor, dynsym->sh_link, &strings_section) ||
        read_string_table(descriptor, strings_section, &strings) ||
        read_symbol_table(descriptor, dynsym, &symbols))
    {
        free(strings_section);
        free((void *)strings);
        free(symbols);
        free(dynsym);
        close(descriptor);

        return errno;
    }

    amount = dynsym->sh_size / sizeof(Elf_Sym);

    /* Trick to get the module base address in a portable way:
     *   Find the first GLOBAL or WEAK symbol in the symbol table,
     *   look this up with dlsym, then return the difference as the base address
     */
    for (i = 0; i < amount; ++i)
    {
        switch(ELF32_ST_BIND(symbols[i].st_info)) {
        case STB_GLOBAL:
        case STB_WEAK:
            found = &symbols[i];
            break;
        default: // Not interested in this symbol
            break;
        }
    }
    if(found != NULL)
    {
        const char *name = &strings[found->st_name];
        void *sym = dlsym(handle, name);
        if(sym != NULL)
            *base = (void*)((size_t)sym - found->st_value);
    }

    free(strings_section);
    free((void *)strings);
    free(symbols);
    free(dynsym);
    close(descriptor);

    return *base == NULL;
}
//--------------------------------------------------------------------------------------------------

void *elf_hook(char const *module_filename, void const *module_address, const std::string& name, void const *substitution)
{
    static size_t pagesize;

    int descriptor;  //file descriptor of shared module

    Elf_Shdr
    *dynsym = NULL,  // ".dynsym" section header
    *rel_plt = NULL,  // ".rel.plt" section header
    *rel_dyn = NULL;  // ".rel.dyn" section header

    Elf_Sym
    *symbol = NULL;  //symbol table entry for symbol named "name"

    Elf_Rel
    *rel_plt_table = NULL,  //array with ".rel.plt" entries
    *rel_dyn_table = NULL;  //array with ".rel.dyn" entries

    std::atomic_size_t
    i,
    name_index,  //index of symbol named "name" in ".dyn.sym"
    rel_plt_amount,  // amount of ".rel.plt" entries
    rel_dyn_amount,  // amount of ".rel.dyn" entries
    *name_address = NULL;  //address of relocation for symbol named "name"

    void *original = NULL;  //address of the symbol being substituted
    if (NULL == module_address || name.empty() || NULL == substitution)
        return original;

    if (!pagesize)
        pagesize = sysconf(_SC_PAGESIZE);

    descriptor = open(module_filename, O_RDONLY);

    if (descriptor < 0)
        return original;


    if (
        section_by_type(descriptor, SHT_DYNSYM, &dynsym) ||  //get ".dynsym" section
        symbol_by_name(descriptor, dynsym, name, &symbol, &name_index) ||  //actually, we need only the index of symbol named "name" in the ".dynsym" table
        section_by_name(descriptor, REL_PLT, &rel_plt) ||  //get ".rel.plt" (for 32-bit) or ".rela.plt" (for 64-bit) section
        section_by_name(descriptor, REL_DYN, &rel_dyn)  //get ".rel.dyn" (for 32-bit) or ".rela.dyn" (for 64-bit) section
       )
    {  //if something went wrong
        free(dynsym);
        free(rel_plt);
        free(rel_dyn);
        free(symbol);
        close(descriptor);
        return original;
    }
//release the data used
    free(dynsym);
    free(symbol);

    rel_plt_table = (Elf64_Rela *)(((size_t)module_address) + rel_plt->sh_addr);  //init the ".rel.plt" array
    rel_plt_amount = rel_plt->sh_size / sizeof(Elf_Rel);  //and get its size

    rel_dyn_table = (Elf_Rel *)(((size_t)module_address) + rel_dyn->sh_addr);  //init the ".rel.dyn" array
    rel_dyn_amount = rel_dyn->sh_size / sizeof(Elf_Rel);  //and get its size
//release the data used
    free(rel_plt);
    free(rel_dyn);
//and descriptor
    close(descriptor);
//now we've got ".rel.plt" (needed for PIC) table and ".rel.dyn" (for non-PIC) table and the symbol's index
    for (i = 0; i < rel_plt_amount; ++i)  //lookup the ".rel.plt" table
        if (ELF64_R_SYM(rel_plt_table[i].r_info) == name_index)  //if we found the symbol to substitute in ".rel.plt"
        {
            original = (void *) *(size_t *) (((size_t) module_address) +
                                             rel_plt_table[i].r_offset);  //save the original function address
            *(size_t *) (((size_t) module_address) +
                         rel_plt_table[i].r_offset) = (size_t) substitution;  //and replace it with the substitutional

            break;  //the target symbol appears in ".rel.plt" only once
        }

    if (original)
        return original;
//we will get here only with 32-bit non-PIC module
    for (i = 0; i < rel_dyn_amount; ++i)  //lookup the ".rel.dyn" table
        if (ELF_R_SYM(rel_dyn_table[i].r_info) == name_index)  //if we found the symbol to substitute in ".rel.dyn"
        {
            name_address = (std::atomic_size_t *) (((size_t) module_address) +
                                       rel_dyn_table[i].r_offset);  //get the relocation address (address of a relative CALL (0xE8) instruction's argument)

            if (!original)
                original = (void *) (*name_address + (size_t) name_address +
                                     sizeof(size_t));  //calculate an address of the original function by a relative CALL (0xE8) instruction's argument

            if (mprotect((void *) (((size_t) name_address) & (((size_t) -1) ^ (pagesize - 1))), pagesize,
                         PROT_READ | PROT_WRITE) < 0)  //mark a memory page that contains the relocation as writable
            {
                return NULL;
            }

            *name_address = (size_t) substitution - (size_t) name_address -
                            sizeof(size_t);  //calculate a new relative CALL (0xE8) instruction's argument for the substitutional function and write it down
            if (mprotect((void *) (((size_t) name_address) & (((size_t) -1) ^ (pagesize - 1))), pagesize,
                         PROT_READ | PROT_EXEC) <
                0)  //mark a memory page that contains the relocation back as executable
            {
                *name_address = (size_t) original - (size_t) name_address -
                                sizeof(size_t);  //if something went wrong then restore the original function address
                return NULL;
            }
        }


    return original;
}

//==================================================================================================
