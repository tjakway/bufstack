#pragma once

#include "NamespaceDefines.hpp"

#include <vector>
#include <utility>
#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackUtil
{
    MsgpackUtil() = delete;
public:
    /**
     * returns a list of the passed objects in reference wrappers
     */
    static std::vector<std::reference_wrapper<msgpack::object>> 
        wrapObjects(const std::vector<msgpack::object>&);
};

BUFSTACK_END_NAMESPACE
