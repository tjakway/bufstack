#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "PrintableObject.hpp"

#include <string>
#include <utility>

BUFSTACK_BEGIN_NAMESPACE

class FdWrapper
    : public Loggable,
    public PrintableObject
{

    int fd;

    void setFd(int _fd)
    {
        fd = _fd;
    }

protected:
    virtual Fields getFields() const noexcept override
    {
        return Fields {
            std::make_pair("fd", std::to_string(fd));
        }
    }

    virtual std::string getName() const noexcept override
    {
        return "FdWrapper";
    }

public:
    static constexpr int initialValue = -1;

    FdWrapper(int _fd)
        : Loggable("FdWrapper"), fd(_fd)
    {}
    
    FdWrapper(const FdWrapper&) = delete;

    FdWrapper(FdWrapper&& other)
        : FdWrapper(other.fd)
    {
        other.setFd(initialValue);
    }

    int getFd() const
    {
        return fd;
    }

    virtual ~FdWrapper()
    {
        if(fd != initialValue && Util::fd_is_valid(getFd()))
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
        setFd(-1);
    }
};


BUFSTACK_END_NAMESPACE
