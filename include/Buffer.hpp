#pragma once

#include <memory>
#include <utility>
#include <cstddef>

#include "NamespaceDefines.hpp"


BUFSTACK_BEGIN_NAMESPACE

class BufDeleter
{
public:
    void operator()(std::pair<char*, std::size_t>* buf)
    {
        delete[] buf->first;
        delete buf;
    }
};
using Buffer = std::unique_ptr<std::pair<char*, std::size_t>, BufDeleter>;

BUFSTACK_END_NAMESPACE
