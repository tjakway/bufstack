#pragma once

#include <functional>

#include <unistd.h>
#include <cstring>

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/Util.hpp"
#include "Util/Strcat.hpp"
#include "Util/FdWrapper.hpp"

BUFSTACK_BEGIN_NAMESPACE

class HasFd
{
public:
    virtual int getFd() const = 0;

    virtual ~HasFd() {}
};

/**
 * HasReadFd and HasWriteFd don't extend HasFd
 * in case they are implemented by separate file descriptors
 */
class HasReadFd
{
public:
    virtual int getReadFd() const = 0;

    virtual ~HasReadFd() {}
};

class HasWriteFd
{
public:
    virtual int getWriteFd() const = 0;

    virtual ~HasWriteFd() {}
};


/**
 * for classes that read/write to the same file descriptor
 */
class HasSingleFd :
    public HasFd,
    virtual public HasReadFd,
    virtual public HasWriteFd
{
    FdWrapper fd;
public:
    HasSingleFd(FdWrapper&& _fd)
        : fd(std::move(_fd))
    {}

    /**
     * default-construct with a -1 file descriptor
     */
    HasSingleFd()
        : HasSingleFd(std::move(FdWrapper()))
    {}

    void setSingleFd(FdWrapper&& _fd)
    {
        fd = std::move(_fd);
    }

    virtual int getFd() const override
    {
        return fd.getFd();
    }

    virtual int getWriteFd() const { return getFd(); }
    virtual int getReadFd() const { return getFd(); }
};

BUFSTACK_END_NAMESPACE
