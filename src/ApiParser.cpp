#include "ApiParser.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include <string>

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


std::string NvimFunction::print(std::string header, 
        //between a field's name and its value
        std::string fieldValueSep,
        //between the field lines themselves
        std::string fieldSep,
        std::string footer) const
{
    return STRCAT(header, 
            "name", fieldValueSep, name, fieldSep,
            "return_type", fieldValueSep, printOptional(returnType), fieldSep,
            "since", fieldValueSep, printOptional(sinceVersion), fieldSep,
            "parameters", fieldValueSep, Util::printVector(parameters),
            footer);
}

std::string NvimFunction::printCompact() const
{
    return print("NvimFunction {", ": ", ", ", "}");
}

std::string NvimFunction::printMultiline() const
{
    return print("NvimFunction\n\t", ": ", "\n\t", "");
}


std::ostream& operator <<(std::ostream& stream, const NvimFunction& f)
{
    stream << f.printCompact();
    return stream;
}


std::vector<std::string> ApiParser::extractFunctionNames(const msgpack::object& h)
{
    std::map<std::string, msgpack::object> apiInfo;

    h.convert(apiInfo);

    std::vector<std::string> keys;

    keys.reserve(apiInfo.size());

    for(auto const& it: apiInfo)
    {
        keys.push_back(it.first);
    }

    return keys;
}

std::unordered_set<NvimFunction> ApiParser::parseFunctions(
        const std::vector<std::reference_wrapper<msgpack::object>>& handles)
{
    std::unordered_set<NvimFunction> parsedFunctions;
    parsedFunctions.reserve(handles.size());

    for(const auto& h : handles)
    {
        parsedFunctions.emplace(parseFunction(h));
    }

    return parsedFunctions;
}


NvimFunction ApiParser::parseFunction(const msgpack::object& h)
{
    std::map<std::string, msgpack::object> function;

    try {
        h.convert(function);
    }
    catch(msgpack::type_error e)
    {
        throw ParseFunctionException(STRCAT("Error converting ", h, 
                    " to an instance of std::map<std::string, msgpack::object>.",
                    "  Exception thrown: ", e.what()));
    }

    optional<std::string> name = tryConvert<std::string>(function.at("name"));

    if(!name.has_value())
    {
        throw ParseFunctionException(STRCAT(
                    "msgpack object ", h, " does not contain"
                    " the key \"name\""));
    }

    //const auto parameters = tryConvert(function.at("parameters"))
    return NvimFunction(tryConvert<std::string>(function.at("return_type")),
                tryConvert<std::string>(function.at("since")),
                tryConvert<std::vector<std::string>>(function.at("parameters"))
                    .value_or(std::vector<std::string>{}),
                name.value());

}

BUFSTACK_END_NAMESPACE
