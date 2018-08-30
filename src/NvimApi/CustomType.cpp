#include "NvimApi/CustomType.hpp"

#include <map>
#include <utility>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

CustomType::Fields CustomType::getFields() const noexcept
{
    return Fields {
        std::make_pair("id", std::to_string(id)),
        std::make_pair("name", name)
    };
}

std::string CustomType::getName() const noexcept
{
    return "CustomType";
}


PrefixType::Fields PrefixType::getFields() const noexcept
{
    //add subclass fields and return
    auto ret = CustomType::getFields();
    ret.emplace(std::make_pair("prefix", prefix));
    return ret;
}

std::string PrefixType::getName() const noexcept
{
    return "PrefixType";
}

std::ostream& operator <<(std::ostream& stream, const CustomType& t)
{
    stream << t.printCompact();
    return stream;
}

std::ostream& operator <<(std::ostream& stream, const PrefixType& t)
{
    stream << t.printCompact();
    return stream;
}

BUFSTACK_END_NAMESPACE
