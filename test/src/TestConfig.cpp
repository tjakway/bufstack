#include "TestConfig.hpp"

#include "NamespaceDefines.hpp"

#include <chrono>

BUFSTACK_BEGIN_NAMESPACE

const std::chrono::milliseconds TestConfig::nvimMaxStartupTime {
    std::chrono::milliseconds(2000)
};

const std::chrono::milliseconds TestConfig::waitBetweenNvimConnectionAttempts {
    std::chrono::milliseconds(100)
};

constexpr uint16_t TestConfig::rpclibTestPort;


BUFSTACK_END_NAMESPACE
