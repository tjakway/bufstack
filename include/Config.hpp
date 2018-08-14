#pragma once

#include "NamespaceDefines.hpp"

#include <chrono>

BUFSTACK_BEGIN_NAMESPACE

class Config
{
public:
    bool forceAsyncWrites;

    class Defaults
    {
    public:
        static const int defaultBacklogSize;
        /*
         * time to sleep between file descriptor reads
         */
        static const std::chrono::milliseconds serverSleepInterval;
    };
};

BUFSTACK_END_NAMESPACE
