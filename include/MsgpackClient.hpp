#pragma once

#include "NamespaceDefines.hpp"
#include "ClientConnection.hpp"
#include "HasFd.hpp"
#include "AbstractMsgpackClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackClient : 
    protected AbstractMsgpackClient,
    virtual public HasClientFd
{
    std::unique_ptr<ClientConnection> clientConnection;

public:
    //tcp connection
    MsgpackClient(ConnectionInfo);

    virtual ~MsgpackClient() {}
};

BUFSTACK_END_NAMESPACE
