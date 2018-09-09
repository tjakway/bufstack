#include "MsgpackClient.hpp"
#include "NamespaceDefines.hpp"

#include "ClientConnection.hpp"
#include "NvimApi/ApiParser.hpp"

BUFSTACK_BEGIN_NAMESPACE


const std::string MsgpackClient::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

MsgpackClient::MsgpackClient(ConnectionInfo ci)
    : clientConnection(ClientConnection::newClientConnection(ci))
{}

MsgpackClient::~MsgpackClient()
{}

void MsgpackClient::onConnect()
{
    std::function<void(void)> init = 
        [this]() -> void {
            msgpack::object_handle apiInfoObject = 
                this->call<("nvim_get_api_info");

            ApiParser parser(apiInfoObject.get());
            ApiInfo apiInfo = parser.getApiInfo();

            //make sure function signatures match what we expect
            std::unordered_set<NvimFunction> functions = parser.getFunctions();
            this->checkFunctions(functions);

            this->initializeRemoteFunctions(apiInfo);
            this->subscribeEvents();
        };

    std::async(std::launch::async, init);
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

BUFSTACK_END_NAMESPACE
