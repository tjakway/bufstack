#pragma once

#include "NamespaceDefines.hpp"
#include "Interruptible.hpp"
#include "Loggable.hpp"

#include <vector>
#include <initializer_list>

BUFSTACK_BEGIN_NAMESPACE

class InterruptibleTask
    : virtual public Loggable,
    public Interruptible
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(InterruptibleTaskException, InterruptibleException);

    const bool logProgressFlag;
    static constexpr bool defaultLogProgress = false;

public:
    using Operations = std::vector<std::function<void(void)>>;

    //master constructor
    InterruptibleTask(
        bool logInterruptCalls,
        bool logProgressFlag,
        Operations operations);

    //pass as a list
    InterruptibleTask(
        Operations operations);

    //initializer list version
    InterruptibleTask(
        std::initializer_list<Operations::value_type> operations);

    virtual void run();

protected:
    Operations operations;

    /**
     * intended to be overridden if better/more detailed logging is desired
     * called BEFORE processing that 
     */
    virtual void logProgress(Operations::iterator);

    virtual void runElement(Operations::iterator);
};

BUFSTACK_END_NAMESPACE
