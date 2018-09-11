#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"

#include <spdlog/spdlog.h>

#include <functional>
#include <string>
#include <sstream>

BUFSTACK_BEGIN_NAMESPACE

class Loggable
{
    std::shared_ptr<spdlog::logger> logger;
    std::string loggerName;

protected:
        template <typename T> 
        std::string toString(const T& x)
        {
            std::ostringstream ss;
            ss << x;
            return ss.str();
        }

public:
        Loggable(const std::string& name, 
                std::function<
                    std::shared_ptr<spdlog::logger>(const std::string&)> 
                        initLogger = Config::Defaults::getLoggerConstructor())
            : loggerName(name), logger(initLogger(name))
        {}


        virtual std::shared_ptr<spdlog::logger> getLogger() const noexcept
        {
            return logger;
        }

};

BUFSTACK_END_NAMESPACE
