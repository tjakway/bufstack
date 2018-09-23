#include "ResumableTask.hpp"

BUFSTACK_BEGIN_NAMESPACE


ResumableTask::ResumableTask(
    bool logInterruptCalls,
    bool logProgress,
    Operations operations)
    : Loggable("ResumableTask"),
        InterruptibleTask(logInterruptCalls,
            logProgress, operations)
{}

//pass as a list
ResumableTask::ResumableTask(
    Operations operations)
    : ResumableTask(
            Interruptible::defaultLogInterruptCalls,
            InterruptibleTask::defaultLogProgress,
            operations)
{}

//initializer list version
ResumableTask::ResumableTask(
    std::initializer_list<Operations::value_type> operations)
    : ResumableTask(Operations(operations))
{}

void ResumableTask::onResume()
{
    if(logProgressFlag)
    {
        getLogger()->debug("onResume() called");
    }
    for(Operations::iterator it = lastRunElement; 
            it != operations.end();
            ++it)
    {
        runElement(it);
    }
}

void ResumableTask::runElement(Operations::iterator it)
{
    lastRunElement = it;
    InterruptibleTask::runElement(it);
}

BUFSTACK_END_NAMESPACE
