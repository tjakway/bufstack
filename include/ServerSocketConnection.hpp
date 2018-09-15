#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

BUFSTACK_BEGIN_NAMESPACE

class ServerSocketConnection
    : virtual public Loggable,
    virtual public Connectible,
    public ServerConnection
{
    sockaddr_in sockaddr;
    int backlog;

protected:
    ServerSocketConnection(int serverFd, sockaddr_in,
            int backlog = Config::serverBacklog);

    virtual void onClientConnect(int clientFd) = 0;

public:
    virtual ~ServerSocketConnection() {}

    virtual void startListen() override;

    NEW_EXCEPTION_TYPE_WITH_BASE(ListenException, BaseException);
};

BUFSTACK_END_NAMESPACE
