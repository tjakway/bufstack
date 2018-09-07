#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class Connectible
{
protected:
    virtual void onConnect(int fd) = 0;
public:
    virtual ~OnConnect() {}

    NEW_EXCEPTION_TYPE(ConnectionException);
    using BaseException = ConnectionException;
};

BUFSTACK_END_NAMESPACE
