#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ServerConnection
    : virtual public Loggable,
    virtual public Connectible
{
protected:
    ServerConnection(int serverFd)
        : Loggable("ServerConnection")
    {}

    virtual void onClientConnect(int clientFd) = 0;

public:
    virtual ~ServerConnection() {}

    NEW_EXCEPTION_TYPE(ServerConnectionException);
    using BaseException = ServerConnectionException;
};

BUFSTACK_END_NAMESPACE
