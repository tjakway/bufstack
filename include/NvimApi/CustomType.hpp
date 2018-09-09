#pragma once

#include "NamespaceDefines.hpp"
#include "Util/Util.hpp"
#include "Util/PrintableObject.hpp"

#include <memory>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

class CustomType : public PrintableObject
{
protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;

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

    virtual ~CustomType() {}

    bool operator==(const CustomType& other) const
    {
        return id == other.id &&
            name == other.name;
    }

    virtual std::unique_ptr<CustomType> clone() const;

    /**
     * equality check ignoring id (which may vary between versions or
     * vim implementations)
     */
    bool nameMatches(const CustomType& other) const noexcept
    {
        return name == other.name;
    }

    bool matches(const CustomType& other) const noexcept
    {
        return matches(other);
    }
};

class PrefixType : public CustomType
{
protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;

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

    virtual ~PrefixType() {}

    bool operator==(const PrefixType& other) const
    {
        return id == other.id &&
            name == other.name &&
            prefix == other.prefix;
    }

    virtual std::unique_ptr<CustomType> clone() const override;
};

std::ostream& operator <<(std::ostream&, const CustomType&);
std::ostream& operator <<(std::ostream&, const PrefixType&);

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
