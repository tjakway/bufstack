#include "MsgpackClient.hpp"
#include "NamespaceDefines.hpp"

#include "ClientConnection.hpp"
#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

BUFSTACK_BEGIN_NAMESPACE


MsgpackClient::MsgpackClient(ConnectionInfo ci)
    : clientConnection(ClientConnection::newClientConnection(ci))
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
