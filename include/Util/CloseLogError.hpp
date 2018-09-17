#pragma once

#include "Loggable.hpp"

#include <cerrno>
#include <cstring>

#define SAFE_CLOSE_LOG_ERROR_LOGGABLE(fd, loggable) \
    if(fd > 0) \
    { \
        if(close(fd) != 0) \
        { \
            auto _errno = errno; \
            loggable.getLogger()->warn(STRCATS("close returned" << \
                " nonzero value when closing file descriptor " << \
                fd << ", error description: " << strerror(_errno))); \
        } \
    }

#define SAFE_CLOSE_LOG_ERROR(fd) SAFE_CLOSE_LOG_ERROR_LOGGABLE(fd, (*this))
