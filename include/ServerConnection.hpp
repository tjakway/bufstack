#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "HasFd.hpp"
#include "Util/FdWrapper.hpp"
#include "Interruptible.hpp"

#include <utility>

BUFSTACK_BEGIN_NAMESPACE

class ServerConnection
    : virtual public Loggable,
    virtual public Connectible,
    public Interruptible
{
    FdWrapper serverFd;
protected:
    ServerConnection(FdWrapper&& _serverFd)
        : Loggable("ServerConnection"),
        serverFd(std::move(_serverFd))
    {}

    int getServerFd() const
    {
        return serverFd.getFd();
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
