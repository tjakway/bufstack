#pragma once

#include "NamespaceDefines.hpp"
#include "Interruptible.hpp"
#include "Loggable.hpp"

#include <vector>

BUFSTACK_BEGIN_NAMESPACE

class InterruptibleTask
    : virtual public Loggable,
    public Interruptible
{
    bool logProgress;

    using Operations = std::vector<std::function<void(void)>>;
    Operations operations;

protected:
    static constexpr bool defaultLogProgress;

    //master constructor
    InterruptibleTask(
        bool logInterruptCalls,
        bool logProgress,
        Operations operations);

    //pass as a list
    InterruptibleTask(
        Operations operations);

    //initializer list version
    InterruptibleTask(
        std::initialize_list<Operations::value_type> operations);

    /**
     * intended to be overridden if better/more detailed logging is desired
     * called BEFORE processing that 
     */
    virtual void logProgress(Operations::iterator);

public:
    void run();
};

BUFSTACK_END_NAMESPACE
