#pragma once

#include "NamespaceDefines.hpp"
#include "HasTcpConnection.hpp"

#include <cstdint>

BUFSTACK_BEGIN_NAMESPACE

class TestConfig
{
public:
    static constexpr auto nvimConnectionAddress = HasTcpConnection::localhost;
    static constexpr uint16_t nvimConnectionPort = 57384; //randomly chosen
};

BUFSTACK_END_NAMESPACE
