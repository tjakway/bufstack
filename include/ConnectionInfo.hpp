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
    };
    const ConnectionType connectionType;

    struct TcpData
    {
        std::string address;
        uint16_t port;
    };

    struct UnixData
    {
        std::string path;
    };

    union Data
    {
        TcpData tcpData;
        UnixData unixData;
    };
    const Data data;

    static ConnectionInfo tcp(
            std::string address,
            uint16_t port);
    static ConnectionInfo unix(std::string path);

protected:
    ConnectionInfo(ConnectionType, Data);

private:
    static Data zeroUnion();
};

BUFSTACK_END_NAMESPACE
