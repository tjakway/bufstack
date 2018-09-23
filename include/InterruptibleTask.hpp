#pragma once

#include "NamespaceDefines.hpp"
#include "Interruptible.hpp"
#include "Loggable.hpp"

#include <vector>
#include <initializer_list>
#include <atomic>

BUFSTACK_BEGIN_NAMESPACE

/**
 * allows you to implement a task as a list of functions,
 * each of which represents an "atom" of work
 * the task can be interrupted before or after any atom has
 * run but once started each will finish before execution stops
 */
class InterruptibleTask
    : virtual public Loggable,
    public Interruptible
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(InterruptibleTaskException, InterruptibleException);

    const bool logProgressFlag;
    static constexpr bool defaultLogProgress = false;

    std::atomic_bool done;

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
    bool isDone() const;

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
