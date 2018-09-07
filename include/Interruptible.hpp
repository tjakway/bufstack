#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

#include "Loggable.hpp"

#include <atomic>

BUFSTACK_BEGIN_NAMESPACE

class Interruptible : virtual public Loggable
{
    std::atomic<bool> flag;

    NEW_EXCEPTION_TYPE(InterruptedException);

public:
    bool interrupted() const noexcept { return flag.load(); }
    void interrupt() { flag.store(true); }

    Interruptible()
        : flag(false)
    {}

    virtual ~Interruptible() {}

protected:
    void throwInterruptedException(std::string msg) const noreturn
    {
        throw InterruptedException(msg);
    }

    void throwInterruptedException() const noreturn
    {
        throw InterruptedException();
    }

    void throwIfInterrupted()
    {
        if(interrupted())
        {
            throwInterruptedException();
        }
    }
};

BUFSTACK_END_NAMESPACE
