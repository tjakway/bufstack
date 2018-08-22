#include "Config.hpp"

BUFSTACK_BEGIN_NAMESPACE

//just should be something large
const int Config::Defaults::defaultBacklogSize = 8192;

const std::chrono::milliseconds Config::Defaults::serverSleepInterval = 
    std::chrono::milliseconds(50);


const std::function<std::shared_ptr<spdlog::logger>(const std::string&)>> mkLogger = 
    [](const std::string& name) { return spdlog::stdout_color_mt(name); };

BUFSTACK_END_NAMESPACE
