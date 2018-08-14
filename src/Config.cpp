#include "Config.hpp"

BUFSTACK_BEGIN_NAMESPACE

//just should be something large
const int Config::Defaults::defaultBacklogSize = 8192;

const std::chrono::milliseconds Config::Defaults::serverSleepInterval = 
    std::chrono::milliseconds(50);

BUFSTACK_END_NAMESPACE
