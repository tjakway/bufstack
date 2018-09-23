#pragma once

#include "Interruptible.hpp"
#include "Loggable.hpp"
#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

/**
 * an Interruptible that can continue later
 */
class Resumable
    : public Interruptible,
      virtual public Loggable
{
protected:
    virtual void onResume() = 0;

public:
    Resumable()
        : Interruptible()
    {}

    Resumable(bool logInterruptCalls)
        : Interruptible(logInterruptCalls)
    {}

    void resume()
    {
        interruptedFlag.store(false);
        onResume();
    }
};

BUFSTACK_END_NAMESPACE
