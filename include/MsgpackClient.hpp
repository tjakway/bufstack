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
    public AbstractMsgpackClient,
    //needed to initialize remote functions
    public std::enable_shared_from_this<MsgpackClient>
{
    const std::unique_ptr<ClientConnection> clientConnection;

    std::unique_ptr<RemoteFunctionInstances> remoteFunctions;
    static const std::string subscribedEvents;


    void onConnect();
protected:
    void initializeRemoteFunctions(const ApiInfo&);
    void subscribeEvents();
    void checkFunctions(const std::unordered_set<NvimFunction>&);

    virtual ClientConnection& getClientConnection() const override;

    MsgpackClient(ConnectionInfo, bool);

public:
    MsgpackClient(ConnectionInfo);

    virtual ~MsgpackClient();
};

BUFSTACK_END_NAMESPACE
