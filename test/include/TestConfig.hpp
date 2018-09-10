#pragma once

#include "NamespaceDefines.hpp"
#include "HasTcpConnection.hpp"

#include <cstdint>
#include <chrono>

BUFSTACK_BEGIN_NAMESPACE

class TestConfig
{
public:
    static constexpr auto nvimConnectionAddress = HasTcpConnection::localhost;
    static constexpr uint16_t nvimConnectionPort = 57384; //randomly chosen

    /**
     * how long we're willing to wait for nvim to startup
     */
    static const std::chrono::milliseconds nvimMaxStartupTime;
    /**
     * only try to connect at most once per this period
     */
    static const std::chrono::milliseconds waitBetweenNvimConnectionAttempts;
};

BUFSTACK_END_NAMESPACE
