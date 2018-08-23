#include "Connect.hpp"

#include "Util/Util.hpp"

#include "ApiParser.hpp"

BUFSTACK_BEGIN_NAMESPACE


void Client::onConnect()
{
    then(client->async_call("vim_get_api_info"),
        [this](msgpack::object_handle apiInfo){
            ApiParser parser(apiInfo);

            //make sure function signatures match what we expect
            std::unordered_set<NvimFunction> functions = parser.getFunctions();
            this->checkFunctions(functions);
        });
}

void Client::checkFunctions(const std::unordered_set<NvimFunction>&)
{
    //TODO
}

BUFSTACK_END_NAMESPACE
