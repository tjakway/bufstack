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

        //we need to subclass InterruptibleTask because just declaring
        //the lambda variables here means they go out of scope before
        //the lambdas finish running
        //alternatively we might be able to use shared_ptrs that are captured by value
        class InterruptibleTaskClosure
            : public InterruptibleTask,
            virtual public Loggable
        {
            //lamda variables
            msgpack::object_handle apiInfoObject;
            std::unique_ptr<ApiParser> parser;
            std::unique_ptr<ApiInfo> apiInfo;
            std::unordered_set<NvimFunction> functions;

        public:
            InterruptibleTaskClosure(NvimClient* client, bool suppressLogging)
                : Loggable("InterruptibleTaskClosure"),
                apiInfoObject(),
                parser(),
                apiInfo(),
                functions(),
                InterruptibleTask(!suppressLogging, 
                    !suppressLogging, InterruptibleTask::Operations {
                    
                    //TODO: consider adding timeouts to any long-running tasks
                    //and checking the interrupt flag before restarting

                    [this, client, suppressLogging](){
                        this->apiInfoObject = client->call<msgpack::object_handle>("nvim_get_api_info");
                        if(!suppressLogging)
                        {
                            client->getLogger()->debug("Received api info object");
                        }
                    },
                    [this, client, suppressLogging](){
                        this->parser = make_unique<ApiParser>(this->apiInfoObject.get());
                        this->parser->suppressLogging(suppressLogging);

                        this->apiInfo = make_unique<ApiInfo>(this->parser->getApiInfo());
                    },
                    [this, client](){
                        this->functions = this->parser->getFunctions();
                    },
                    [this, client](){
                        client->checkFunctions(this->functions);
                    },
                    [this, client](){
                        client->initializeRemoteFunctions(*this->apiInfo);
                    },
                    [this, client](){
                        client->subscribeEvents();
                    }
                })
            {}

            virtual ~InterruptibleTaskClosure() {}
        };

        onConnectTask = make_unique<InterruptibleTaskClosure>(this, suppressLogging);

        //run the onConnect interruptible task
        onConnectFuture = make_unique<std::future<void>>(
                std::async(std::launch::async,
                    std::bind(&InterruptibleTask::run, onConnectTask.get())));
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
            //wait for the thread to clean up before proceeding
            onConnectFuture->wait();
        }
    }
}

BUFSTACK_END_NAMESPACE
