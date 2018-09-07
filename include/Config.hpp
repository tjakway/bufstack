#pragma once

#include "NamespaceDefines.hpp"

#include <spdlog/spdlog.h>

#include <chrono>
#include <functional>
#include <memory>

#include <cstddef> //std::size_t

BUFSTACK_BEGIN_NAMESPACE

class Config
{
public:
    bool forceAsyncWrites;

    class Defaults
    {
    public:
        static const std::size_t defaultBacklogSize;
        /*
         * time to sleep between file descriptor reads
         */
        static const std::chrono::milliseconds serverSleepInterval;

        /**
         * default logger configuration
         */
        static std::function<std::shared_ptr<spdlog::logger>(const std::string&)> getLoggerConstructor();
    };
};

BUFSTACK_END_NAMESPACE
