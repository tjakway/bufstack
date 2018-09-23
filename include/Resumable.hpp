#pragma once

#include "Interruptible.hpp"
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
    void onResume() = 0;

public:
    Resumable()
        : Interruptible()
    {}

    Resumable(bool logInterruptCalls)
        : Interrruptible(logInterruptCalls)
    {}

    void resume()
    {
        interruptedFlag.store(false);
        onResume();
    }
};

BUFSTACK_END_NAMESPACE
