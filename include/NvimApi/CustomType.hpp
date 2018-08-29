#pragma once

#include "NamespaceDefines.hpp"

#include <string>

BUFSTACK_BEGIN_NAMESPACE

class CustomType
{
public:
    const int id;
    const std::string prefix;
    const std::string name;

    CustomType(int _id, 
            const std::string& _prefix,
            const std::string& _name)
        : id(_id), prefix(_prefix),
        name(_name)
    {}

    CustomType(const CustomType& other)
        : CustomType(other.id,
                other.prefix, other.name)
    {}
};

BUFSTACK_END_NAMESPACE
