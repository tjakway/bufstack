#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "NewExceptionType.hpp"

#include <msgpack.hpp>

#include <map>
#include <set>
#include <vector>
#include <string>
#include <algorithm>

#include "nonstd/optional.hpp"

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class NvimFunction
{
public:
    const optional<std::string> returnType;
    const optional<std::string> sinceVersion;
    const std::vector<std::string> parameters;
    const std::string name;

    NvimFunction(const optional<std::string> _returnType,
            const optional<std::string> _sinceVersion,
            const std::vector<std::string> _parameters,
            const std::string _name)
        : returnType(_returnType), sinceVersion(_sinceVersion),
        parameters(_parameters), name(_name)
    {}

    //copy constructor
    NvimFunction(const NvimFunction& other)
        : returnType(other.returnType),
        sinceVersion(other.sinceVersion),
        parameters(other.parameters),
        name(other.name)
    {}

    virtual ~NvimFunction() {}

    bool operator==(const NvimFunction& other) const 
    {
        return returnType == other.returnType &&
            sinceVersion == other.sinceVersion &&
            parameters == other.parameters &&
            name == other.name;
    }
};

class ApiParser : public Loggable
{
    const msgpack::object_handle& handle;

protected:
    NEW_EXCEPTION_TYPE(ApiParserException);

public:
    static bool isFunctionObject(const msgpack::object_handle& h)
    {
        std::map<std::string, msgpack::object_handle> functions;


    }

    static bool keysAreFunctionObject(const std::set<std::string>& keys)
    {
        const std::set<std::string> expectedKeys = {
            "return_type", "since", "method",
            "parameters", "name"
        };

        return std::includes(keys.begin(), keys.end(),
                expectedKeys.begin(), expectedKeys.end());
    }

    static std::vector<std::string> extractFunctionNames(const msgpack::object_handle&);

    static std::vector<NvimFunction> parseFunctions(const msgpack::object_handle&);

public:
    ApiParser(const msgpack::object_handle& _handle)
        : handle(_handle)
    {}


};

BUFSTACK_END_NAMESPACE
