#include "NvimClient.hpp"

#include <functional>

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

BUFSTACK_BEGIN_NAMESPACE

const std::string NvimClient::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

std::future<void>& NvimClient::asyncOnConnect(bool suppressLogging)
{
    asyncStartListening(getClientConnection().getReadFd());



    //make sure that calling this method multiple times
    //will only generate one actual request to the server
    if(!onConnectFuture)
    {

        //lamda variables
        msgpack::object_handle apiInfoObject;
        std::unique_ptr<ApiParser> parser;
        std::unique_ptr<ApiInfo> apiInfo;
        std::unordered_set<NvimFunction> functions;

        onConnectTask = make_unique<InterruptibleTask>(!suppressLogging, 
                !suppressLogging, InterruptibleTask::Operations {
            [this, &apiInfoObject, suppressLogging](){
                apiInfoObject = this->call<msgpack::object_handle>("nvim_get_api_info");
                if(!suppressLogging)
                {
                    this->getLogger()->debug("Received api info object");
                }
            },
            [this, &apiInfo, &apiInfoObject, &parser, suppressLogging](){
                parser = make_unique<ApiParser>(apiInfoObject.get());
                parser->suppressLogging(suppressLogging);

                apiInfo = make_unique<ApiInfo>(parser->getApiInfo());
            },
            [this, &functions, &parser](){
                functions = parser->getFunctions();
            },
            [this, &functions](){
                this->checkFunctions(functions);
            },
            [this, &apiInfo](){
                this->initializeRemoteFunctions(*apiInfo);
            },
            [this](){
                this->subscribeEvents();
            }
        });

        //run the onConnect interruptible task
        onConnectFuture = make_unique<std::future<void>>(
                std::async(std::bind(&InterruptibleTask::run, onConnectTask.get())));
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
        asyncOnConnect(suppressLogging);
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
    if(onConnectFuture && onConnectTask)
    {
        if(!onConnectTask->interrupted()
                && !onConnectTask->isDone())
        {
            onConnectTask->interrupt();
        }
    }
}

BUFSTACK_END_NAMESPACE
