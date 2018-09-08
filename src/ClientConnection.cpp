#include "ClientConnection.hpp"

#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

std::unique_ptr<ClientConnection> 
        ClientConnection::newClientConnection(ConnectionInfo i)
{
    switch(i.connectionType)
    {
        case ConnectionInfo::Tcp:
            return make_unique<ClientTcpConnection>(
                    i.tcpData.address, i.tcpData.port);
        case ConnectionInfo::Unix:
            return make_unique<ClientUnixConnection>(i.unixData.path);

        default:
            throw ClientConnectionException(
                STRCATS("Unrecognized ConnectionInfo::Type " <<
                    i.type));
    }
}

BUFSTACK_END_NAMESPACE
