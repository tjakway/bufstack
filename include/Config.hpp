#pragma once

#include "NamespaceDefines.hpp"

#include <spdlog/spdlog.h>

#include <chrono>
#include <functional>
#include <memory>

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

        /**
         * default logger configuration
         */
        static const 
            std::function<std::shared_ptr<spdlog::logger>(const std::string&)> mkLogger;
    };
};

BUFSTACK_END_NAMESPACE
