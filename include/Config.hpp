#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class Config
{
public:
    bool forceAsyncWrites;

    class Defaults
    {
    public:
        static const int defaultBacklogSize;
    };
};

BUFSTACK_END_NAMESPACE
