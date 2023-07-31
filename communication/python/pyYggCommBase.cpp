#include "utils/enums.hpp"
#include "communicators/CommBase.hpp"


namespace communication {
namespace communicator {
void Comm_t::reset() {

}
Comm_t* Comm_t::create_worker(utils::Address *adr, const DIRECTION &dir, int flgs) {
    (void)adr;
    (void)dir;
    (void)flgs;

    return nullptr;
}
int Comm_t::send_single(const char *data, const size_t &len, const utils::Header &header) {
    (void)data;
    (void)len;
    (void)header;
    return -1;
}

long Comm_t::recv_single(char *&data, const size_t &len, bool allow_realloc) {
    (void)data;
    (void)len;
    (void)allow_realloc;
    return -1;
}

int Comm_t::comm_nmsg() const {
    return 0;
}

void Comm_t::close() {

}
bool Comm_t::is_closed() const {

    return false;
}

}
}
