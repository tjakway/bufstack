#include "ApiParser.hpp"

#include "Util/Strcat.hpp"

namespace {
template<typename T> optional<T> tryConvert(const msgpack::object& h)
{
    T t;
    try
    {
        h.convert(t);
        return t;
    }
    catch(msgpack::type_error)
    {
        return nullopt;
    }
}
}


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

std::unordered_set<NvimFunction> ApiParser::parseFunctions(const std::vector<msgpack::object_handle>& handles)
{
    std::unordered_set<NvimFunction> parsedFunctions;
    parsedFunctions.reserve(handles.size());

    for(const auto& h : handles)
    {
        parsedFunctions.emplace(parseFunction(h));
    }

    return parsedFunctions;
}


NvimFunction ApiParser::parseFunction(const msgpack::object_handle& h)
{
    std::map<std::string, msgpack::object> function;

    try {
        h.get().convert(function);
    }
    catch(msgpack::type_error e)
    {
        throw ParseFunctionException(STRCAT("Error converting ", h.get(), 
                    " to an instance of std::map<std::string, msgpack::object>.",
                    "  Exception thrown: ", e.what()));
    }

    optional<std::string> name = tryConvert<std::string>(function.at("name"));

    if(!name.has_value())
    {
        throw ParseFunctionException(STRCAT(
                    "msgpack object ", h.get(), " does not contain"
                    " the key \"name\""));
    }

    //const auto parameters = tryConvert(function.at("parameters"))
    return NvimFunction(tryConvert<std::string>(function.at("returnType")),
                tryConvert<std::string>(function.at("since")),
                tryConvert<std::vector<std::string>>(function.at("parameters"))
                    .value_or(std::vector<std::string>{}),
                name.value());

}

BUFSTACK_END_NAMESPACE
