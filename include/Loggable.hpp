#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"

#include <spdlog/spdlog.h>

#include <functional>

BUFSTACK_BEGIN_NAMESPACE

class Loggable
{
    std::shared_ptr<spdlog::logger> logger;
    std::string loggerName;

protected:
        Loggable(const std::string& name, 
                std::function<
                    std::shared_ptr<spdlog::logger>(const std::string&)> 
                        initLogger = Config::Defaults::mkLogger)
            : loggerName(name), logger(initLogger(name))
        {}

        virtual std::shared_ptr<spdlog::logger> getLogger()
        {
            return logger;
        }
};

BUFSTACK_END_NAMESPACE
