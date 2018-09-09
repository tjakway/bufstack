#pragma once

#include "NamespaceDefines.hpp"
#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE

/**
 * currently a work in progress
 * TODO: 
 *  -make CustomType a subclass of NvimType
 *  -integrate with NvimFunction and NvimFunctionSpec
 */
class NvimType
{
    const msgpack::msgpack_object_type msgpackType;

public:
    NvimType(msgpack::msgpack_object_type type)
        : msgpackType(type)
    {}

    NvimType(const NvimType& other)
        : NvimType(other.msgpackType)
    {}
};

BUFSTACK_END_NAMESPACE
