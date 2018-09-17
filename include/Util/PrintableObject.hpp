#pragma once

#include "NamespaceDefines.hpp"
#include "nonstd/optional.hpp"

#include <iostream>
#include <string>
#include <map>

BUFSTACK_BEGIN_NAMESPACE

class PrintableObject
{
protected:
    using Fields = std::map<std::string, std::string>;
    virtual Fields getFields() const noexcept = 0;
    virtual std::string getName() const noexcept = 0;

    static const std::string defaultCompactFieldValueSep,
                 defaultCompactFieldSep,
                 defaultCompactFooter;

    static const std::string defaultMultilineFieldValueSep,
                 defaultMultilineFieldSep,
                 defaultMultilineFooter;

public:
    virtual std::string getDefaultHeader() const noexcept;
    virtual std::string getDefaultFieldValueSep() const noexcept;
    virtual std::string getDefaultFieldSep() const noexcept;
    virtual std::string getDefaultFooter() const noexcept;

    virtual std::string print(
        nonstd::optional<std::string> header = nonstd::nullopt,
        //between a field's name and its value
        nonstd::optional<std::string> fieldValueSep = nonstd::nullopt,
        //between the field lines themselves
        nonstd::optional<std::string> fieldSep = nonstd::nullopt,
        nonstd::optional<std::string> footer = nonstd::nullopt) const;

    virtual std::string printCompact() const;
    virtual std::string printMultiline() const;
};

#define PRINTABLE_OBJECT_OVERLOAD_STREAM_OPERATOR(className) \
    std::ostream& operator <<(std::ostream& s, const className& c) \
    { \
        s << c.printCompact();\
        return s; \
    }

BUFSTACK_END_NAMESPACE
