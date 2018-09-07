#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class OnConnect
{
protected:
    virtual void onConnect(int fd) = 0;
public:
    virtual ~OnConnect() {}
};

BUFSTACK_END_NAMESPACE
