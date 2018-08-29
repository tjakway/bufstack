#include "Client.hpp"

#include "Util/Util.hpp"

#include "NvimApi/ApiParser.hpp"

BUFSTACK_BEGIN_NAMESPACE


void Client::onConnect()
{
    /*
    then(client->async_call("vim_get_api_info"),
        [this](msgpack::object_handle apiInfo) -> std::future<void> {
            ApiParser parser(apiInfo);

            //make sure function signatures match what we expect
            std::unordered_set<NvimFunction> functions = parser.getFunctions();
            this->checkFunctions(functions);

            return std::future<void>{};
        });*/
}

void Client::checkFunctions(const std::unordered_set<NvimFunction>&)
{
    //TODO
}

BUFSTACK_END_NAMESPACE
