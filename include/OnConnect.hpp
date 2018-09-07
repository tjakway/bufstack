#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

BUFSTACK_BEGIN_NAMESPACE

class Connectible
{
protected:
    virtual void onConnect(int fd) = 0;
    NEW_EXCEPTION_TYPE(ConnectionException);

public:
    virtual ~Connectible() {}

    using BaseException = ConnectionException;
};

BUFSTACK_END_NAMESPACE
