#include "Client.hpp"

#include "Util/Util.hpp"

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/RemoteFunction.hpp"

#include <msgpack/object_decl.hpp>
#include <msgpack/object_fwd.hpp>
#include <msgpack/object_fwd_decl.hpp>
#include <msgpack/pack_decl.hpp>
#include <msgpack.hpp>

#include <future>

BUFSTACK_BEGIN_NAMESPACE

const std::string Client::subscribedEvents = 
        "BufEnter,BufLeave,TabEnter,TabLeave,WinEnter,WinLeave";

void Client::onConnect()
{
    std::function<void(void)> init = 
        [this]() -> void {
            msgpack::object_handle apiInfoObject = client->call("nvim_get_api_info");

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

void Client::initializeRemoteFunctions(
        const ApiInfo& apiInfo)
{
    remoteFunctions = make_unique<RemoteFunctionInstances>(
            client, apiInfo);
}

void Client::checkFunctions(const std::unordered_set<NvimFunction>&)
{
    //TODO
}

void Client::subscribeEvents()
{
    remoteFunctions->subscribe(subscribedEvents);
}

BUFSTACK_END_NAMESPACE
