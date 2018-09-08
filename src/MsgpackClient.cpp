#include "MsgpackClient.hpp"
#include "NamespaceDefines.hpp"

#include "ClientConnection.hpp"

BUFSTACK_BEGIN_NAMESPACE

MsgpackClient::MsgpackClient(ConnectionInfo ci)
    : clientConnection(ClientConnection::newClientConnection(ci))
{}

BUFSTACK_END_NAMESPACE
