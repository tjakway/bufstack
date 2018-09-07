#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

#include "Loggable.hpp"

#include <atomic>

BUFSTACK_BEGIN_NAMESPACE

class Interruptible : virtual public Loggable
{
    std::atomic<bool> flag;
    bool logInterruptCalls;

    NEW_EXCEPTION_TYPE(InterruptedException);

protected:
    Interruptible(bool _flag, bool _logInterruptCalls)
        : flag(_flag), logInterruptCalls(_logInterruptCalls)
    {}

public:
    bool interrupted() const noexcept { return flag.load(); }
    void interrupt() 
    { 
        if(logInterruptCalls)
        {
            getLogger()->debug("interrupt() called");
        }
        flag.store(true); 
    }

    Interruptible()
        : Interruptible(false, true)
    {}

    Interruptible(bool _logInterruptCalls)
        : Interruptible(false, _logInterruptCalls)
    {}

    //make this class abstract
    //see https://stackoverflow.com/questions/1219607/why-do-we-need-a-pure-virtual-destructor-in-c
    virtual ~Interruptible() = 0;

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
