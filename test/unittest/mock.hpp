#pragma once
#include <sys/msg.h>

#define LIBRARY_ADDRESS_BY_HANDLE(dlhandle) ((NULL == dlhandle) ? NULL :  (void*)*(size_t const*)(dlhandle))
#define SUBLIB "/home/friedel/crops_in_silico/yggcpp/cmake-build-release/libYggInterface.so"

#define ELFHOOK(x) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::x);

#define ELFREVERT(x,y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, y);

namespace communication {
namespace mock {
//extern int mock_method_return_value;
//void setValue(const int val);
extern int RETVAL;
extern int SENDCOUNT;

int msgsnd(int a, const void* b, size_t c, int d);

int msgctl(int h, int flag, msqid_ds *buf);

int msgget(key_t a, int b);

ssize_t msgrcv(int a, void* buf, size_t msz, long mtype, int flags);

void* realloc(void* ptr, size_t size);
}
}