#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class HasTcpConnection
{
    const std::string address;
    const uint16_t port;

public:
    HasTcpConnection(
            const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port)
    {}

    virtual ~HasTcpConnection() {}

    static constexpr auto localhost = "127.0.0.1";
};

BUFSTACK_END_NAMESPACE
