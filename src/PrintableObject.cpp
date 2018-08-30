#include "PrintableObject.hpp"

#include <sstream>

BUFSTACK_BEGIN_NAMESPACE

const std::string PrintableObject::defaultFieldValueSep = ": ";
const std::string PrintableObject::defaultFieldSep = ", ";
const std::string PrintableObject::defaultFooter = "}";

std::string PrintableObject::getDefaultHeader() const noexcept
{
    return getName() + " {";
}

std::string PrintableObject::getDefaultFieldValueSep() const noexcept
{
    return defaultFieldValueSep;
}

std::string PrintableObject::getDefaultFieldSep() const noexcept
{
    return defaultFieldSep;
}

std::string PrintableObject::getDefaultFooter() const noexcept
{
    return defaultFooter;
}

std::string PrintableObject::print(
        nonstd::optional<std::string> header,
        //between a field's name and its value
        nonstd::optional<std::string> fieldValueSep,
        //between the field lines themselves
        nonstd::optional<std::string> fieldSep,
        nonstd::optional<std::string> footer) const
{
    std::ostringstream ss;
    ss << header.value_or(getDefaultHeader());

    for(const auto& it : getFields())
    {
        ss << it.first << 
            fieldValueSep.value_or(getDefaultFieldValueSep()) << 
            it.second << 
            fieldSep.value_or(getDefaultFieldSep());
    }

    ss << footer.value_or(getDefaultFooter());

    return ss.str();
}

BUFSTACK_END_NAMESPACE
