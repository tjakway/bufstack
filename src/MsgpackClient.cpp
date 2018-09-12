#include "MsgpackClient.hpp"
#include "NamespaceDefines.hpp"

#include "ClientConnection.hpp"
#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

namespace {
    class MsgpackOnConnectThread 
    {

    };
}

BUFSTACK_BEGIN_NAMESPACE


const std::string MsgpackClient::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

MsgpackClient::MsgpackClient(ConnectionInfo ci, bool skipOnConnect)
    : clientConnection(ClientConnection::newClientConnection(ci))
{
    if(!skipOnConnect)
    {
        //since the client connection is constructed, we've already connected
        onConnect();
    }
}

MsgpackClient::MsgpackClient(ConnectionInfo ci)
    : MsgpackClient(ci, false)
{}

MsgpackClient::~MsgpackClient()
{}

void MsgpackClient::onConnect()
{
    /*
    std::function<void(void)> init = 
        [this]() -> void {*/
        this->getLogger()->set_level(spdlog::level::debug);
    
        msgpack::object_handle apiInfoObject = 
            this->call<msgpack::object_handle>("nvim_get_api_info");

        this->getLogger()->debug("Received api info object");

        ApiParser parser(apiInfoObject.get());
        ApiInfo apiInfo = parser.getApiInfo();

        //make sure function signatures match what we expect
        std::unordered_set<NvimFunction> functions = parser.getFunctions();
        this->checkFunctions(functions);

        this->initializeRemoteFunctions(apiInfo);
        this->subscribeEvents();
    //};

    //std::async(init).wait();
//    std::future<void> x = then<msgpack::object_handle, std::function<void(msgpack::object_handle)>>
//        (apiInfoCall, init);
}

void MsgpackClient::initializeRemoteFunctions(
        const ApiInfo& apiInfo)
{
    remoteFunctions = make_unique<RemoteFunctionInstances>(
            shared_from_this(), apiInfo);
}

void MsgpackClient::checkFunctions(const std::unordered_set<NvimFunction>&)
{
    //TODO
}

void MsgpackClient::subscribeEvents()
{
    remoteFunctions->subscribe(subscribedEvents);
}


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
