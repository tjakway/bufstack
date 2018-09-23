#include "ResumableTask.hpp"

BUFSTACK_BEGIN_NAMESPACE

void ResumableTask::resume()
{
    for(Operations it = lastRunElement; 
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
