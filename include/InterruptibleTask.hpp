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

    using Operations = std::vector<std::function<void(void)>>;
    Operations operations;


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

    /**
     * intended to be overridden if better/more detailed logging is desired
     * called BEFORE processing that 
     */
    virtual void logProgress(Operations::iterator);

    virtual void runElement(Operations::iterator);

public:
    virtual void run();
};

BUFSTACK_END_NAMESPACE
