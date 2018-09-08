#pragma once

#include "NamespaceDefines.hpp"

#include <string>
#include <cstdint>

BUFSTACK_BEGIN_NAMESPACE

class ConnectionInfo
{
public:
    enum ConnectionType {
        Tcp = 1,
        /**
         * unix domain sockets
         */
        Unix = 2
    } connectionType;

    struct TcpData
    {
        std::string address;
        uint16_t port;
    };

    struct UnixData
    {
        std::string path;
    };

    union data
    {
        TcpData tcpData;
        UnixData unixData;
    };
};

BUFSTACK_END_NAMESPACE
