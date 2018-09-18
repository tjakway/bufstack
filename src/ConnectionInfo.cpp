#include "ConnectionInfo.hpp"

#include <cstring>

BUFSTACK_BEGIN_NAMESPACE

ConnectionInfo::ConnectionInfo(
    ConnectionType _type, Data _data)
    : connectionType(_type), data(_data)
{}

ConnectionInfo ConnectionInfo::tcpConnection(
    std::string address,
    uint16_t port)
{
    Data d;
    d.tcpData.address = address;
    d.tcpData.port = port;

    return ConnectionInfo(ConnectionType::Tcp, d);
}

ConnectionInfo ConnectionInfo::unixConnection(std::string path)
{
    Data d;
    d.unixData.path = path;

    return ConnectionInfo(ConnectionType::Unix, d);
}

ConnectionInfo ConnectionInfo::embeddedConnection(int readFd, int writeFd)
{
    Data d;
    d.pipeData.readFd = readFd;
    d.pipeData.writeFd = writeFd;

    return ConnectionInfo(ConnectionType::Embedded, d);
}

BUFSTACK_END_NAMESPACE
