#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/NewExceptionType.hpp"

#include <msgpack.hpp>

#include <functional>
#include <map>
#include <set>
#include <vector>
#include <unordered_set>
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

BUFSTACK_END_NAMESPACE

//from https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
size_t hash_combine( size_t lhs, size_t rhs ) {
  lhs^= rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2);
  return lhs;
}

//hash specialization
namespace std
{
    template<> struct hash<bufstack::NvimFunction>
    {
        typedef bufstack::NvimFunction argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const& f) const noexcept
        {
            return hash_combine(std::hash<const optional<std::string>>{}(f.returnType),
                    hash_combine(std::hash<const optional<std::string>>{}(f.sinceVersion),
                    hash_combine(std::hash<const std::vector<std::string>>{}(f.parameters),
                        std::hash<const std::string>{}(f.name));
        }
    };
}

BUFSTACK_BEGIN_NAMESPACE

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

    static std::unordered_set<NvimFunction> parseFunctions(
            const std::vector<msgpack::object_handle>&);
    static NvimFunction parseFunction(const msgpack::object_handle&);

public:
    ApiParser(const msgpack::object_handle& _handle)
        : handle(_handle)
    {}


};

BUFSTACK_END_NAMESPACE
