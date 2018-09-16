#include "NvimClient.hpp"

namespace {

    class MsgpackOnConnectThread 
    {

    };
}

BUFSTACK_BEGIN_NAMESPACE

const std::string MsgpackClient::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

void NvimClient::onConnect()
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

/**
    * skipOnConnect: whether to call onConnect()
    */
NvimClient::NvimClient(ConnectionInfo ci, bool skipOnConnect)
    : Loggable("NvimClient"),
    MsgpackClient(ci)
{
    if(!skipOnConnect)
    {
        //since the client connection is constructed, we've already connected
        onConnect();
    }
}

NvimClient::NvimClient(ConnectionInfo ci)
{

}

NvimClient::~NvimClient()
{

}

BUFSTACK_END_NAMESPACE
