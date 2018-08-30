#include "PrintableObject.hpp"

#include <sstream>

BUFSTACK_BEGIN_NAMESPACE

const std::string PrintableObject::defaultCompactFieldValueSep = ": ";
const std::string PrintableObject::defaultCompactFieldSep = ", ";
const std::string PrintableObject::defaultCompactFooter = "}";

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


std::string PrintableObject::printCompact() const
{
    //use default parameters
    return print(nonstd::make_optional(getHeader()), 
            nonstd::make_optional(defaultCompactFieldValueSep),
            nonstd::make_optional(defaultCompactFieldSep),
            nonstd::make_optional(defaultCompactFooter));
}

std::string PrintableObject::printMultiline() const
{
    return print(nonstd::make_optional(getHeader()), 
            nonstd::make_optional(defaultMultilineFieldValueSep),
            nonstd::make_optional(defaultMultilineFieldSep),
            nonstd::make_optional(defaultMultilineFooter));
}

BUFSTACK_END_NAMESPACE
