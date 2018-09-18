#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "HasFd.hpp"
#include "Interruptible.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ServerConnection
    : virtual public Loggable,
    virtual public Connectible,
    public Interruptible,
    public HasServerFd
{
protected:
    ServerConnection(int serverFd)
        : Loggable("ServerConnection")
    {
        setServerFd(serverFd);
    }

    virtual void onClientConnect(int clientFd) = 0;

public:
    virtual ~ServerConnection() {}

    NEW_EXCEPTION_TYPE(ServerConnectionException);
    using BaseException = ServerConnectionException;

    /**
     * named startListen to disambiguate it from listen(2)
     */
    virtual void startListen() = 0;
};

BUFSTACK_END_NAMESPACE