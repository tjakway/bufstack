#pragma once

#include <functional>

#include <unistd.h>
#include <cstring>

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

class HasFd
{
public:
    HasFd(int _fd)
        : fd(_fd)
    {}

    HasFd()
        : fd(-1)
    {}

    virtual int getFd() const = 0;

    virtual ~HasFd()
    {}
};

class HasReadFd : public HasFd
{
public:
    int getReadFd() { return getFd(); }
    void setReadFd(int fd) { setFd(fd); }

    HasReadFd(int _fd)
        : HasFd(_fd)
    {}

    HasReadFd()
        : HasFd()
    {}

    virtual ~HasReadFd() {}
};

class HasWriteFd : public HasFd
{
public:
    int getWriteFd() { return getFd(); }
    void setWriteFd(int fd) { setFd(fd); }

    HasWriteFd(int _fd)
        : HasFd(_fd)
    {}

    HasWriteFd()
        : HasFd()
    {}

    virtual ~HasWriteFd() {}
};


BUFSTACK_END_NAMESPACE
