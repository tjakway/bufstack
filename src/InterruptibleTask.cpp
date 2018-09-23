#include "InterruptibleTask.hpp"

#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

constexpr bool InterruptibleTask::defaultLogProgress = false;

//master constructor
InterruptibleTask::InterruptibleTask(
    bool logInterruptCalls,
    bool _logProgress,
    Operations _operations)
    : Interruptible(logInterruptCalls),
    logProgress(_logProgress),
    operations(_operations)
{}

//pass as list
InterruptibleTask::InterruptibleTask(
    Operations _operations)
    : InterruptibleTask(defaultLogInterruptCalls,
        _logProgress,
        _operations)
{}

//initializer list version
InterruptibleTask::InterruptibleTask(
    std::initialize_list<Operations::value_type> operations)
    : InterruptibleTask(defaultLogInterruptCalls,
            defaultLogProgress,
            Operations(operations))
{}

void InterruptibleTask::logProgress(Operations::iterator it)
{
    getLogger()->debug(
        STRCATS("About to process element " << 
            std::distance(it - operations.begin())));
}

void InterruptibleTask::run()
{
    for(auto it = operations.begin();
            it != operations.end();
            ++it)
    {
        if(!interrupted())
        {
            //log each element then run it
            if(logProgress)
            {
                logProgress(it);
            }
            (*it)();
        }
    }
}


BUFSTACK_END_NAMESPACE
