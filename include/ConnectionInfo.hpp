#pragma once

#include "NamespaceDefines.hpp"

#include <string>
#include <cstring>
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
        TcpData() = default;
        TcpData(const TcpData&) = default;
        ~TcpData() {}
    };

    struct UnixData
    {
        std::string path;
        UnixData() = default;
        UnixData(const UnixData&) = default;
        ~UnixData() {}
    };

    union Data
    {
        TcpData tcpData;
        UnixData unixData;

        //see https://stackoverflow.com/questions/321351/initializing-a-union-with-a-non-trivial-constructor
        Data() { memset(this, 0, sizeof(Data)); }
        Data(const Data& other) { memcpy(this, &other, sizeof(Data)); }
        ~Data() {}
    };
    const Data data;

    static ConnectionInfo tcpConnection(
            std::string address,
            uint16_t port);
    static ConnectionInfo unixConnection(std::string path);

protected:
    ConnectionInfo(ConnectionType, Data);
};

BUFSTACK_END_NAMESPACE