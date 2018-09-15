#include "ServerSocketConnection.hpp"

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "Util/Strcat.hpp"

#include <sys/types.h>

#include <cerrno>

BUFSTACK_BEGIN_NAMESPACE

ServerSocketConnection::ServerSocketConnection(int serverFd, 
        sockaddr_in _sockaddr,
        int _backlog)
    : Loggable("ServerSocketConnection"),
    ServerConnection(serverFd),
    sockaddr(_sockaddr),
    backlog(_backlog)
{}

void ServerSocketConnection::startListen()
{
    if(bind(getServerFd(), 
        (struct sockaddr*)&sockaddr,
        sizeof(sockaddr)) != 0)
    {
        auto _errno = errno;
        throw ListenException(STRCATS(
            "Error when calling bind(2): " <<
            strerror(_errno)));
    }

    if(listen(getServerFd(), backlog) != 0)
    {
        auto _errno = errno;
        throw ListenException(STRCATS(
            "Error when calling listen(2): " <<
            strerror(_errno)));
    }

    start
}

BUFSTACK_END_NAMESPACE
