#pragma once

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"

#include <msgpack.hpp>

#include <map>
#include <set>
#include <vector>
#include <string>
#include <algorithm>

#include "nonstd/optional.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ApiParser : public Loggable
{
    const msgpack::object_handle& handle;

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

public:
    ApiParser(const msgpack::object_handle& _handle)
        : handle(_handle)
    {}


};

BUFSTACK_END_NAMESPACE
