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

std::future<void> NvimClient::asyncOnConnect(bool suppressLogging)
{
    asyncStartListening(getClientConnection().getReadFd());

    //make sure that calling this method multiple times
    //will only generate one actual request to the server
    if(!onConnectFuture)
    {
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

        onConnectFuture = make_unique(std::move(std::async(init)));
        return *onConnectFuture;
    }
    //if we've already connected then return a reference to the future
    //we still have
    else
    {
        getLogger()->warn("onConnectFuture != nullptr: "
                "asyncOnConnect has already been called");
        return *onConnectFuture;
    }
}

void NvimClient::syncOnConnect(bool suppressLogging)
{
    asyncOnConnect(suppressLogging).wait();
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
    //TODO
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
