#pragma once

#include <functional>

#include <unistd.h>
#include <cstring>

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

class HasFd : virtual public Loggable
{
    int fd;

protected:
    int getFd() const noexcept
    {
        if(!Util::fd_is_valid(fd))
        {
            getLogger()->warn(
                STRCATS("Util::fd_is_valid(fd) returned" <<
                    " false for file descriptor " << fd));
        }
        return fd;
    }

    void setFd(int newFd) noexcept
    {
        fd = newFd;
    }

public:
    HasFd(int _fd)
        : fd(_fd)
    {}

    HasFd()
        : fd(-1)
    {}

    virtual ~HasFd()
    {
        if(Util::fd_is_valid(getFd()))
        {
            int res = close(getFd());
            if(res != 0)
            {
                auto _errno = errno;
                getLogger()->warn(STRCATS("close returned" <<
                    " nonzero value when closing file descriptor " <<
                    fd << ", error description: " << strerror(_errno)));
            }
        }
    }
};

class HasClientFd : public HasFd
{
public:
    int getClientFd() { return getFd(); }
    void setClientFd(int fd) { setFd(fd); }

    HasClientFd(int _fd)
        : HasFd(_fd)
    {}

    HasClientFd()
        : HasFd()
    {}
};

class HasServerFd : public HasFd
{
public:
    int getServerFd() { return getFd(); }
    void setServerFd(int fd) { setFd(fd); }

    HasServerFd(int _fd)
        : HasFd(_fd)
    {}

    HasServerFd()
        : HasFd()
    {}
};

BUFSTACK_END_NAMESPACE
