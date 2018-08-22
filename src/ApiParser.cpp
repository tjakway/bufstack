#include "ApiParser.hpp"

#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

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


std::string NvimFunction::print(std::string header, 
        //between a field's name and its value
        std::string fieldValueSep,
        //between the field lines themselves
        std::string fieldSep,
        std::string footer) const
{
    std::ostringstream ss;
    ss << header << 
            "name" << fieldValueSep << name << fieldSep <<
            "method" << fieldValueSep << printOptional(method) << fieldSep <<
            "return_type" << fieldValueSep << printOptional(returnType) << fieldSep <<
            "since" << fieldValueSep << printOptional(sinceVersion) << fieldSep <<
            "parameters" << fieldValueSep << Util::printVector(parameters) <<
            footer;

    return ss.str();
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


const std::string ApiParser::Keys::ApiInfo::functions = "functions";
const std::string ApiParser::Keys::ApiInfo::types = "types";
const std::string ApiParser::Keys::ApiInfo::version = "version";
const std::string ApiParser::Keys::ApiInfo::errorTypes = "error_types";

const std::set<std::string> ApiParser::Keys::ApiInfo::keys {
    functions, types, version, errorTypes
};

const std::string ApiParser::Keys::Function::returnType = "return_type";
const std::string ApiParser::Keys::Function::since = "since";
const std::string ApiParser::Keys::Function::method = "method";
const std::string ApiParser::Keys::Function::parameters = "parameters";
const std::string ApiParser::Keys::Function::name = "name";

const std::set<std::string> ApiParser::Keys::Function::keys {
    returnType, since, method, parameters, name
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
    return Util::leftIncludesRight(keys, Keys::Function::keys);
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


NvimFunction ApiParser::ParseFunctions::parseNvimFunction(const msgpack::object& h)
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

    optional<std::string> name = tryConvert<std::string>(function.at(Keys::Function::name));

    if(!name.has_value())
    {
        throw ParseFunctionException(STRCAT(
                    "msgpack object ", h, " does not contain"
                    " the key \"name\""));
    }

    return NvimFunction(tryConvert<bool>(function.at(Keys::Function::method)),
                tryConvert<std::string>(function.at(Keys::Function::returnType)),
                tryConvert<std::string>(function.at(Keys::Function::since)),
                tryConvert<std::vector<std::string>>(function.at(Keys::Function::parameters))
                    .value_or(std::vector<std::string>{}),
                name.value());

}

BUFSTACK_END_NAMESPACE
