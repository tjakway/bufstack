#include "InterruptibleTask.hpp"

#include "Util/Strcat.hpp"

#include <algorithm>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

//master constructor
InterruptibleTask::InterruptibleTask(
    bool logInterruptCalls,
    bool _logProgress,
    Operations _operations)
    : Loggable("InterruptibleTask"),
    Interruptible(logInterruptCalls),
    logProgressFlag(_logProgress),
    operations(_operations),
    done(false)
{}

//pass as list
InterruptibleTask::InterruptibleTask(
    Operations _operations)
    : InterruptibleTask(defaultLogInterruptCalls,
        defaultLogProgress,
        _operations)
{}

//initializer list version
InterruptibleTask::InterruptibleTask(
    std::initializer_list<Operations::value_type> operations)
    : InterruptibleTask(defaultLogInterruptCalls,
            defaultLogProgress,
            Operations(operations))
{}

void InterruptibleTask::logProgress(Operations::iterator it)
{
    getLogger()->debug(
        STRCATS("About to process element " << 
            std::to_string(std::distance(operations.begin(), it))));
}


void InterruptibleTask::runElement(Operations::iterator it)
{
    (*it)();
}

void InterruptibleTask::run()
{
    if(interrupted())
    {
        throw InterruptibleTaskException("run() called but have already been interrupted");
    }

    for(auto it = operations.begin();
            it != operations.end();
            ++it)
    {
        if(!interrupted())
        {
            //log each element then run it
            if(logProgressFlag)
            {
                logProgress(it);
            }
            runElement(it);
        }
    }

    if(!interrupted())
    {
        done.store(true);
    }
}

bool InterruptibleTask::isDone() const
{
    return done.load();
}

void InterruptibleTask::interrupt()
{
    if(isDone())
    {
        getLogger()->warn("interrupt() called after completion");
        Interruptible::interrupt();
    }
}


BUFSTACK_END_NAMESPACE
