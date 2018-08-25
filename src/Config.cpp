#include "Config.hpp"

#include <spdlog/sinks/stdout_color_sinks.h>

#include <string>
#include <cstdlib>

BUFSTACK_BEGIN_NAMESPACE

//just should be something large
const int Config::Defaults::defaultBacklogSize = 8192;

const std::chrono::milliseconds Config::Defaults::serverSleepInterval = 
    std::chrono::milliseconds(50);

//changed from a field because of https://isocpp.org/wiki/faq/ctors#static-init-order;
std::function<std::shared_ptr<spdlog::logger>(const std::string&)> Config::Defaults::getLoggerConstructor()
{
    const std::function<std::shared_ptr<spdlog::logger>(const std::string&)> 
        loggerConstructor = 
            [](const std::string& name) { 
                std::string genName = 
                    name + std::string("_instance_") + std::to_string(rand());
                return spdlog::stdout_color_mt(genName); 
            };

    return loggerConstructor;
}

BUFSTACK_END_NAMESPACE
