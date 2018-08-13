#include "ApiParser.hpp"

BUFSTACK_BEGIN_NAMESPACE


std::vector<std::string> ApiParser::extractFunctionNames(const msgpack::object_handle& h)
{
    std::map<std::string, msgpack::object> apiInfo;

    h.get().convert(apiInfo);

    std::vector<std::string> keys;

    keys.reserve(apiInfo.size());

    for(auto const& it: apiInfo)
    {
        keys.push_back(it.first);
    }

    return keys;
}

BUFSTACK_END_NAMESPACE
