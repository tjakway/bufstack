#pragma once

#include "NamespaceDefines.hpp"
#include "ClientConnection.hpp"
#include "HasFd.hpp"
#include "AbstractMsgpackClient.hpp"

#include "NvimApi/NvimFunction.hpp"
#include "NvimApi/RemoteFunction_decl.hpp"
#include "NvimApi/ApiInfo_decl.hpp"

#include <memory>
#include <unordered_set>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackClient : 
    public AbstractMsgpackClient
{
    const std::unique_ptr<ClientConnection> clientConnection;

protected:
    virtual ClientConnection& getClientConnection() const override;

public:
    MsgpackClient(ConnectionInfo);

    virtual ~MsgpackClient();
};

BUFSTACK_END_NAMESPACE
