#pragma once

#include "NamespaceDefines.hpp"
#include "InterruptibleTask.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ResumableTask
    : virtual public Loggable,
    public Resumable,
    public InterruptibleTask
{
    Operations::iterator lastRunElement;
public:
    //master constructor
    ResumableTask(
        bool logInterruptCalls,
        bool logProgress,
        Operations operations);

    //pass as a list
    ResumableTask(
        Operations operations);

    //initializer list version
    ResumableTask(
        std::initialize_list<Operations::value_type> operations);

    virtual void runElement(Operations::iterator it) override;

    void resume();
};

BUFSTACK_END_NAMESPACE
