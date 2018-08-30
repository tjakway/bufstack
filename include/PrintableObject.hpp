#pragma once

#include "NamespaceDefines.hpp"
#include "nonstd/optional.hpp"

#include <string>
#include <map>

BUFSTACK_BEGIN_NAMESPACE

class PrintableObject
{
protected:
    virtual std::map<std::string, std::string> getFields() const noexcept = 0;
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

BUFSTACK_END_NAMESPACE
