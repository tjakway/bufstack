#include "NvimApi/ApiParser.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include "nonstd/optional.hpp"

#include <sstream>
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


const std::string ApiParser::Keys::ApiInfo::functions = "functions";
const std::string ApiParser::Keys::ApiInfo::types = "types";
const std::string ApiParser::Keys::ApiInfo::version = "version";
const std::string ApiParser::Keys::ApiInfo::errorTypes = "error_types";

const std::set<std::string> ApiParser::Keys::ApiInfo::keys = {
    functions, types, version, errorTypes
};

const std::string ApiParser::Keys::Function::returnType = "return_type";
const std::string ApiParser::Keys::Function::since = "since";
const std::string ApiParser::Keys::Function::deprecatedSince = "deprecated";
const std::string ApiParser::Keys::Function::method = "method";
const std::string ApiParser::Keys::Function::parameters = "parameters";
const std::string ApiParser::Keys::Function::name = "name";

std::set<std::string> ApiParser::Keys::Function::getMandatoryKeys() {
    return std::set<std::string>{ returnType, since, method, parameters, name };
};

std::set<std::string> ApiParser::Keys::Function::getAllKeys() {
    std::set<std::string> keys = getMandatoryKeys();

    //insert optional keys
    keys.insert(deprecatedSince);

    return keys;
}

const std::string ApiParser::Keys::Type::id = "id";
const std::string ApiParser::Keys::Type::prefix = "prefix";
const std::set<std::string> ApiParser::Keys::Type::keys = {
    id, prefix
};

void ApiParser::parseApiInfo(const std::vector<msgpack::object_handle>& vecH)
{
    if(!vecH.size())
    {
        throw ParseApiInfoException(STRCATS("Expected api info message to" <<
                " have 1 msgpack object but size == " << vecH.size()));
    }
    else
    {
        try {
            std::map<std::string, msgpack::object> apiInfo;
            vecH.at(0).get().convert(apiInfo);

            std::vector<msgpack::object> functionObjects;
            apiInfo.at(Keys::ApiInfo::functions).convert(functionObjects);

            std::vector<std::reference_wrapper<msgpack::object>> refs;
            for(auto& it : functionObjects)
            {
                refs.emplace_back(std::reference_wrapper<msgpack::object>(it));
            }

            functions = parseFunctions.parseNvimFunctions(refs);
        } catch(msgpack::type_error e)
        {
            throw ParseApiInfoException(STRCAT("Caught msgpack::type_error in ", 
                        __func__, ": ", e.what()));
        }
    }
}

bool ApiParser::ParseFunctions::keysAreApiInfo(const std::set<std::string>& keys)
{
    return Util::leftIncludesRight(keys, Keys::ApiInfo::keys);
}

bool ApiParser::ParseFunctions::keysAreFunctionObject(const std::set<std::string>& keys)
{
    return Util::leftIncludesRight(keys, Keys::Function::getMandatoryKeys());
}


std::vector<std::string> ApiParser::ParseFunctions::extractFunctionNames(const msgpack::object& h)
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

std::unordered_set<NvimFunction> ApiParser::ParseFunctions::parseNvimFunctions(
        const std::vector<std::reference_wrapper<msgpack::object>>& handles)
{
    std::unordered_set<NvimFunction> parsedFunctions;
    parsedFunctions.reserve(handles.size());

    for(const auto& h : handles)
    {
        const auto f = parseNvimFunction(h);
        getLogger()->info("parsed function {}", f.printCompact());
        parsedFunctions.emplace(f);
    }

    return parsedFunctions;
}

std::unordered_set<CustomType> ApiParser::ParseFunctions::parseCustomTypes(
        const std::vector<std::reference_wrapper<msgpack::object>>& handles)
{
    std::unordered_set<CustomType> parsedTypes;
    parsedTypes.reserve(handles.size());

    for(const auto& h : handles)
    {
        const auto f = parseCustomType(h);
        getLogger()->info("parsed custom type {}", f.printCompact());
        parsedTypes.emplace(f);
    }

    return parsedTypes;

}


CustomType ApiParser::ParseFunctions::parseCustomType(const msgpack::object& h)
{
    std::map<std::string, msgpack::object> msgpackType;


    try {
        h.convert(msgpackType);
    }
    catch(msgpack::type_error e)
    {
        //TODO: DRY, see parseNvimFunction
        throw ParseCustomTypeException(STRCAT("Error converting ", h, 
                    " to an instance of std::map<std::string, msgpack::object>.",
                    "  Exception thrown: ", e.what()));
    }

    optional<int> id = tryConvert(function.at())
}

NvimFunction ApiParser::ParseFunctions::parseNvimFunction(const msgpack::object& h)
{
    std::map<std::string, msgpack::object> function;

    try {
        h.convert(function);
    }
    catch(msgpack::type_error e)
    {
        //TODO: DRY, see parseCustomType 
        throw ParseFunctionException(STRCAT("Error converting ", h, 
                    " to an instance of std::map<std::string, msgpack::object>.",
                    "  Exception thrown: ", e.what()));
    }

    optional<std::string> name = tryConvert<std::string>(function.at(Keys::Function::name));

    if(!name.has_value())
    {
        throw ParseFunctionException(STRCAT(
                    "msgpack object ", h, " does not contain"
                    " the key \"name\""));
    }

    optional<std::string> deprecated;

    std::map<std::string, msgpack::object>::iterator it = 
        function.find(Keys::Function::deprecatedSince);
    if(it == function.end())
    {
        //function isn't deprecated
        deprecated = nullopt;
    }
    else
    {
        std::string deprecatedSinceVersion = "NOT DEPRECATED";
        it->second.convert(deprecatedSinceVersion);
        deprecated = make_optional(deprecatedSinceVersion);
    }

    getLogger()->set_level(spdlog::level::debug);
    std::ostringstream ss;
    ss << function.at(Keys::Function::parameters);
    getLogger()->debug("parameters object for {}: {}", printOptional(name),
            ss.str());

    return NvimFunction(tryConvert<bool>(function.at(Keys::Function::method)),
                tryConvert<std::string>(function.at(Keys::Function::returnType)),
                tryConvert<std::string>(function.at(Keys::Function::since)),
                deprecated,
                tryConvert<std::vector<std::string>>(function.at(Keys::Function::parameters))
                    .value_or(std::vector<std::string>{}),
                name.value());

}

BUFSTACK_END_NAMESPACE