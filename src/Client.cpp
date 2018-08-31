#include "Client.hpp"

#include "Util/Util.hpp"

#include "NvimApi/ApiParser.hpp"

BUFSTACK_BEGIN_NAMESPACE

const std::string Client::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

void Client::onConnect()
{
    std::future<msgpack::object_handle> apiInfoCall = client->async_call("nvim_get_api_info");
    std::function<void(msgpack::object_handle)> init = 
        [this](msgpack::object_handle apiInfoObject) -> void {
            ApiParser parser(apiInfoObject.get());
            ApiInfo apiInfo = parser.getApiInfo();

            //make sure function signatures match what we expect
            std::unordered_set<NvimFunction> functions = parser.getFunctions();
            this->checkFunctions(functions);

            this->initializeRemoteFunctions(apiInfo);
            this->subscribeEvents();

        };

    std::future<void> x = then<msgpack::object_handle, std::function<void(msgpack::object_handle)>>
        (apiInfoCall, init);
}

void Client::initializeRemoteFunctions(
        const ApiInfo& apiInfo)
{
    remoteFunctions = make_unique<RemoteFunctionInstances>(
            shared_from_this(), apiInfo);
}

void Client::checkFunctions(const std::unordered_set<NvimFunction>&)
{
    //TODO
}

void Client::subscribeEvents()
{
}

BUFSTACK_END_NAMESPACE
