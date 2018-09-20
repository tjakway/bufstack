#pragma once

#include "NamespaceDefines.hpp"
#include "ClientConnection.hpp"
#include "HasFd.hpp"
#include "MsgpackClient.hpp"
#include "Loggable.hpp"

#include "NvimApi/NvimFunction.hpp"
#include "NvimApi/RemoteFunction_decl.hpp"
#include "NvimApi/ApiInfo_decl.hpp"

#include <memory>
#include <unordered_set>

BUFSTACK_BEGIN_NAMESPACE

class NvimClient : 
    public MsgpackClient,
    //needed to initialize remote functions
    public std::enable_shared_from_this<NvimClient>,
    virtual public Loggable
{
    std::unique_ptr<RemoteFunctionInstances> remoteFunctions;
    static const std::string subscribedEvents;
    void onConnect();

protected:
    void initializeRemoteFunctions(const ApiInfo&);
    void subscribeEvents();
    void checkFunctions(const std::unordered_set<NvimFunction>&);

    /**
     * skipOnConnect: whether to call onConnect()
     */
    NvimClient(ConnectionInfo, bool skipOnConnect);
    NvimClient(std::shared_ptr<ClientConnection>, bool);

public:
    NvimClient(ConnectionInfo);

    virtual ~NvimClient();
};

BUFSTACK_END_NAMESPACE
