#pragma once

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