#include <fcntl.h>
#include <unistd.h>

int c_fcntl_getfl(int fd) {
    return fcntl(fd, F_GETFL);
}

int c_fcntl_setfl(int fd, int flags) {
    return fcntl(fd, F_SETFL, flags);
}
