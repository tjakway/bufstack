#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

#include "Loggable.hpp"

#include <atomic>

BUFSTACK_BEGIN_NAMESPACE

class Interruptible : virtual public Loggable
{
protected:
    std::atomic<bool> interruptedFlag;
    bool logInterruptCalls;

    NEW_EXCEPTION_TYPE(InterruptibleException);
    NEW_EXCEPTION_TYPE_WITH_BASE(InterruptedException, InterruptibleException);

    Interruptible(bool _interruptedFlag, bool _logInterruptCalls)
        : interruptedFlag(_interruptedFlag), logInterruptCalls(_logInterruptCalls)
    {}

    static constexpr bool defaultLogInterruptCalls = false;

public:
    bool interrupted() const noexcept { return interruptedFlag.load(); }
    virtual void interrupt() 
    { 
        if(logInterruptCalls)
        {
            getLogger()->debug("interrupt() called");
        }
        interruptedFlag.store(true); 
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
    [[noreturn]] void throwInterruptedException(std::string msg) const
    {
        throw InterruptedException(msg);
    }

    [[noreturn]] void throwInterruptedException() const
    {
        throw InterruptedException("");
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
