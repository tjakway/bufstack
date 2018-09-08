#include "ConnectionInfo.hpp"

#include <cstring>

BUFSTACK_BEGIN_NAMESPACE

Data ConnectionInfo::zeroUnion()
{
    Data data;
    memset(&data, 0, sizeof(Data));
    return data;
}

ConnectionInfo::ConnectionInfo(
    ConnectionType _type, Data _data)
    : connectionType(_type), data(_data)
{}

ConnectionInfo ConnectionInfo::tcpConnection(
    std::string address,
    uint16_t port)
{
    auto d = zeroUnion();
    d.tcpData.address = address;
    d.tcpData.port = port;

    return ConnectionInfo(type, d);
}

ConnectionInfo unixConnection(std::string path)
{
    auto d = zeroUnion();
    d.unixData.path = path;

    return ConnectionInfo(type, d);
}

BUFSTACK_END_NAMESPACE
