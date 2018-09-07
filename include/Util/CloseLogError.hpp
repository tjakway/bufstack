#pragma once

#include "Loggable.hpp"

#include <cerrno>
#include <cstring>

#define CLOSE_LOG_ERROR(fd) \
    if(fd > 0) { \
        if(close(getFd())res != 0) \
        { \
            auto _errno = errno; \
            getLogger()->warn(STRCATS("close returned" << \
                " nonzero value when closing file descriptor " << \
                fd << ", error description: " << strerror(_errno))); \
        } \
    }
