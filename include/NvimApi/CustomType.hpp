#pragma once

#include "NamespaceDefines.hpp"
#include "Util/Util.hpp"

#include <string>

BUFSTACK_BEGIN_NAMESPACE

class CustomType
{
protected:
    virtual std::string printFields(const std::string& divider) const;

public:
    const int id;
    const std::string name;

    CustomType(int _id, 
            const std::string& _name)
        : id(_id),
        name(_name)
    {}

    CustomType(const CustomType& other)
        : CustomType(other.id, other.name)
    {}

    bool operator==(const CustomType& other)
    {
        return id == other.id &&
            name == other.name;
    }

    virtual std::string printCompact() const;
    virtual std::string printMultiline() const;
};

class PrefixType : public CustomType
{
protected:
    virtual std::string printFields(const std::string& divider) const override;

public:
    const std::string prefix;

    PrefixType(int _id,
            const std::string& _name,
            const std::string& _prefix)
        : CustomType(_id, _name), prefix(_prefix)
    {}

    PrefixType(const PrefixType& other)
        : PrefixType(other.id, other.name,
                other.prefix)
    {}

    bool operator==(const PrefixType& other)
    {
        return id == other.id &&
            name == other.name &&
            prefix == other.prefix;
    }
};

BUFSTACK_END_NAMESPACE


//hash specialization
namespace std
{
    template<> struct hash<bufstack::CustomType>
    {
        typedef bufstack::CustomType argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const& f) const noexcept
        {
            return hash_combine(std::hash<int>{}(f.id),
                    std::hash<std::string>{}(f.name));
        }
    };

    template<> struct hash<bufstack::PrefixType>
    {
        typedef bufstack::PrefixType argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const& f) const noexcept
        {
            //hash the supertype and combine with subtype-specific fields
            return hash_combine(
                    std::hash<bufstack::CustomType>{}(
                        static_cast<bufstack::CustomType>(f)),
                    std::hash<std::string>{}(f.name));
        }
    };
}
