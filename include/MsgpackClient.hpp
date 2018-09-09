#pragma once

#include "NamespaceDefines.hpp"
#include "ClientConnection.hpp"
#include "HasFd.hpp"
#include "AbstractMsgpackClient.hpp"

#include "NvimApi/RemoteFunction.hpp"

#include <memory>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackClient : 
    public AbstractMsgpackClient,
    virtual public HasClientFd,
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

public:
    MsgpackClient(ConnectionInfo);

    virtual ~MsgpackClient() {}
};

BUFSTACK_END_NAMESPACE
