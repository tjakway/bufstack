#include "NvimClient.hpp"

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

namespace {

    class MsgpackOnConnectThread 
    {

    };
}

BUFSTACK_BEGIN_NAMESPACE

const std::string NvimClient::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

void NvimClient::onConnect(bool suppressLogging)
{
    asyncStartListening(getClientConnection().getReadFd());

    std::function<void(void)> init = 
        [this, suppressLogging]() -> void {
    
        msgpack::object_handle apiInfoObject = 
            this->call<msgpack::object_handle>("nvim_get_api_info");

        if(!suppressLogging)
        {
            this->getLogger()->debug("Received api info object");
        }

        ApiParser parser(apiInfoObject.get());
        parser.suppressLogging(suppressLogging);

        ApiInfo apiInfo = parser.getApiInfo();

        //make sure function signatures match what we expect
        std::unordered_set<NvimFunction> functions = parser.getFunctions();
        this->checkFunctions(functions);

        this->initializeRemoteFunctions(apiInfo);
        this->subscribeEvents();
    };

    std::async(init).wait();
//    std::future<void> x = then<msgpack::object_handle, std::function<void(msgpack::object_handle)>>
//        (apiInfoCall, init);

}

void NvimClient::initializeRemoteFunctions(const ApiInfo& apiInfo)
{
    remoteFunctions = make_unique<RemoteFunctionInstances>(
            shared_from_this(), apiInfo);
}

void NvimClient::subscribeEvents()
{
    remoteFunctions->subscribe(subscribedEvents);
}

void NvimClient::checkFunctions(const std::unordered_set<NvimFunction>&)
{

}


NvimClient::NvimClient(std::shared_ptr<ClientConnection> conn, 
        bool skipOnConnect, bool suppressLogging)
    : Loggable("NvimClient"),
    MsgpackClient(conn)
{
    if(!skipOnConnect)
    {
        //since the client connection is constructed, we've already connected
        onConnect(suppressLogging);
    }
}


/**
    * skipOnConnect: whether to call onConnect()
    */
NvimClient::NvimClient(ConnectionInfo ci, bool skipOnConnect, bool suppressLogging)
    : NvimClient(
            std::shared_ptr<ClientConnection>(
                ClientConnection::newClientConnection(ci)), skipOnConnect)
{}

NvimClient::NvimClient(ConnectionInfo ci)
    : NvimClient(ci, false)
{}

NvimClient::~NvimClient()
{

}

BUFSTACK_END_NAMESPACE
