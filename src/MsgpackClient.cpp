#include "MsgpackClient.hpp"
#include "NamespaceDefines.hpp"

#include "ClientConnection.hpp"
#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

BUFSTACK_BEGIN_NAMESPACE


MsgpackClient::MsgpackClient(std::shared_ptr<ClientConnection> conn)
    : clientConnection(conn)
{}

MsgpackClient::MsgpackClient(ConnectionInfo ci)
    : MsgpackClient(
            std::shared_ptr<ClientConnection>(
                ClientConnection::newClientConnection(ci)))
{}

MsgpackClient::~MsgpackClient()
{}

ClientConnection& MsgpackClient::getClientConnection() const
{
    if(!clientConnection)
    {
        throw ClientConnection::ClientConnectionException(
                "Client connection pointer is null");
    }
    else
    {
        return *clientConnection;
    }
}

BUFSTACK_END_NAMESPACE
