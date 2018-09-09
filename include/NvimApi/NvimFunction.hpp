#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "Util/PrintableObject.hpp"

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

using ApiInfoPacket = msgpack::object;

class NvimFunction;

class NvimFunctionSpec : public PrintableObject
{
    const bool method,
    const optional<std::string> returnType;
    const std::vector<std::string> parameters;
    const std::string name;

protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;

public:
    NvimFunctionSpec(bool _method,
            //nullopt = void return type
            optional<std::string> _returnType,
            std::vector<std::string> _parameters = 
                std::vector<std::string>{},
            std::string _name)
        : method(_method), returnType(_returnType),
        parameters(_parameters), name(_name)
    {}

    NvimFunctionSpec(const NvimFunctionSpec& other)
        : NvimFunctionSpec(other.method,
                other.returnType, other.parameters,
                other.name)
    {}

    virtual ~NvimFunctionSpec() {}

    NvimFunction toFunction() const noexcept;

    bool matches(const NvimFunctionSpec&) const noexcept;
    bool matches(const NvimFunction&) const noexcept;
};

class NvimFunction : public PrintableObject
{
protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;

public:
    const optional<bool> method;
    const optional<std::string> returnType;
    const optional<std::string> sinceVersion;
    const optional<std::string> deprecatedSince;
    const std::vector<std::string> parameters;
    const std::string name;

    NvimFunction(const optional<bool> _method,
            const optional<std::string> _returnType,
            const optional<std::string> _sinceVersion,
            const optional<std::string> _deprecatedSince,
            const std::vector<std::string> _parameters,
            const std::string _name)
        : method(_method), 
        returnType(_returnType), sinceVersion(_sinceVersion),
        deprecatedSince(_deprecatedSince),
        parameters(_parameters), name(_name)
    {}

    //copy constructor
    NvimFunction(const NvimFunction& other)
        : method(other.method),
        returnType(other.returnType),
        sinceVersion(other.sinceVersion),
        deprecatedSince(other.deprecatedSince),
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

    /**
     * equality ignoring irrelevant fields
     */
    bool matches(const NvimFunction& other) const
    {
        return returnType == other.returnType && 
            parameters == other.parameters &&
            name == other.name;
    }

    bool deprecated() const noexcept { return deprecatedSince.has_value(); }

    bool method() const noexcept { return method.value_or(false) };
};

std::ostream& operator <<(std::ostream&, const NvimFunction&);


BUFSTACK_END_NAMESPACE

//hash all items in a vector and return them
template<typename T> size_t hash_vector(std::vector<T> v)
{
    size_t acc = 0;
    for(const T& t : v)
    {
        acc = hash_combine(acc, std::hash<T>{}(t));
    }

    return acc;
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
            return hash_combine(std::hash<optional<std::string>>{}(f.returnType),
                    hash_combine(std::hash<optional<std::string>>{}(f.sinceVersion),
                    hash_combine(hash_vector(f.parameters),
                        std::hash<std::string>{}(f.name))));
        }
    };
}
