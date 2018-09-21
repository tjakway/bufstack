#include "NvimApi/ApiParser.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include "nonstd/optional.hpp"

#include <iterator>
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

void ApiParser::parseApiInfo(const msgpack::object& o)
{
    try {
        std::map<std::string, msgpack::object> apiInfo;
        o.convert(apiInfo);

        //parse functions
        std::vector<msgpack::object> functionObjects;
        apiInfo.at(Keys::ApiInfo::functions).convert(functionObjects);

        std::vector<std::reference_wrapper<msgpack::object>> refs;
        for(auto& it : functionObjects)
        {
            refs.emplace_back(std::reference_wrapper<msgpack::object>(it));
        }

        functions = parseFunctions.parseNvimFunctions(refs);
        
        //begin parsing custom types
        const auto insertAll = [this](
                ApiParser::CustomTypeSet& thisSet) {

            for(const auto& thisElem : thisSet)
            {
                this->customTypes.emplace(thisElem);
            }
        };

        //parse error types
        std::map<std::string, msgpack::object> errorTypeObjects;
        apiInfo.at(Keys::ApiInfo::errorTypes).convert(errorTypeObjects);
        auto errTypesSet = parseFunctions.parseCustomTypes(errorTypeObjects);
        insertAll(errTypesSet);

        std::map<std::string, msgpack::object> regTypeObjects;
        apiInfo.at(Keys::ApiInfo::types).convert(regTypeObjects);
        auto regTypesSet = parseFunctions.parseCustomTypes(regTypeObjects);
        insertAll(regTypesSet);

    } catch(msgpack::type_error e)
    {
        throw ParseApiInfoException(STRCAT("Caught msgpack::type_error in ", 
                    __func__, ": ", e.what()));
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
        getLogger()->debug("parsed function {}", f.printCompact());
        parsedFunctions.emplace(f);
    }

    return parsedFunctions;
}

ApiParser::CustomTypeSet 
    ApiParser::ParseFunctions::parseCustomTypes(
        const std::map<std::string, msgpack::object>& handles)
{
    ApiParser::CustomTypeSet parsedTypes;
    parsedTypes.reserve(handles.size());

    for(const auto& h : handles)
    {
        auto f = parseCustomType(h.first, h.second);
        if(!f)
        {
            throw ParseCustomTypeException(STRCATS("Got null pointer while parsing " <<
                        "{ key = " << h.first << ", value = " << 
                        h.second << " } as a msgpack custom type"));
        }
        else
        {
            getLogger()->debug("parsed custom type {}", f->printCompact());
            parsedTypes.emplace(std::move(f));
        }
    }

    return parsedTypes;
}


std::shared_ptr<CustomType> ApiParser::ParseFunctions::parseCustomType(
        const std::string& name,
        const msgpack::object& h)
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

    optional<int> id = tryConvert<int>(msgpackType.at(ApiParser::Keys::Type::id));
    
    if(!id.has_value())
    {
        throw ParseCustomTypeException(STRCATS(
                    "Error parsing " << h << " as a msgpack custom type," <<
                    " expected nonexistant field " <<
                    ApiParser::Keys::Type::id));
    }

    optional<std::string> prefix;
    //extract the prefix if this type has one
    if(msgpackType.find(ApiParser::Keys::Type::prefix) != msgpackType.end())
    {
        prefix = tryConvert<std::string>(
                msgpackType.at(ApiParser::Keys::Type::prefix));
    }

    //return a PrefixType if the object has a prefix field
    if(prefix.has_value())
    {
        return std::make_shared<PrefixType>(id.value(), name, prefix.value());
    }
    else
    {
        return std::make_shared<CustomType>(id.value(), name);
    }

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


    const auto nameValue = name.value();

    return NvimFunction(tryConvert<bool>(function.at(Keys::Function::method)),
                tryConvert<std::string>(function.at(Keys::Function::returnType)),
                tryConvert<std::string>(function.at(Keys::Function::since)),
                deprecated,
                parseParameters(function.at(Keys::Function::parameters), nameValue), 
                nameValue);

}

std::vector<std::string> ApiParser::ParseFunctions::parseParameters(const msgpack::object& obj,
        const std::string& name)
{
    //parse parameters
    std::ostringstream ss;
    ss << obj;
    getLogger()->debug("parameters object for {}: {}", name,
            ss.str());


    //for some reason the parameters are wrapped twice: instead of a list of objects,
    //it's actually a list of list of objects
    //unwrap it
    const std::vector<msgpack::object>& paramObjList = 
        obj.as<std::vector<msgpack::object>>();

    std::vector<std::string> parsedParams;

    for(const msgpack::object& thisElem : paramObjList)
    {
        parsedParams.emplace_back(thisElem.as<std::vector<std::string>>().at(0));
    }

    return parsedParams;
}

std::unordered_set<NvimFunction> ApiParser::getFunctions()
{
    return functions;
}

ApiParser::CustomTypeSet ApiParser::getCustomTypes()
{
    return customTypes;
}

ApiInfo ApiParser::getApiInfo()
{
    return ApiInfo(functions, customTypes);
}

ApiParser::ApiParser(const msgpack::object& o)
    : Loggable("ApiParser")
{
    parseApiInfo(o);
}

void ApiParser::suppressLogging(bool flag)
{
    if(flag)
    {
        getLogger()->set_level(spdlog::level::warn);
    }
    else
    {
        getLogger()->set_level(spdlog::level::info);
    }
}

BUFSTACK_END_NAMESPACE
