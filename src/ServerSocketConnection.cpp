#include "ServerSocketConnection.hpp"

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "Util/Strcat.hpp"

#include <sys/types.h>

#include <cstdlib>
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
    //bind the socket and wait for a connection
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

    while(!interrupted())
    {
        //wait for a connection then hand it off to the callback
        sockaddr_in client;
        memset(&client, 0 sizeof(sockaddr_in));
        socklen_t clientLen = sizeof(sockaddr_in);
        int clientFd = accept(server_fd, (struct sockaddr *) &client, &clientLen);

        if(clientFd < 0)
        {
            auto _errno = errno;
            throw ListenException(STRCATS(
                "Error when calling accept(2): " <<
                strerror(_errno)));
        }

        onClientConnect(clientFd);
    }
}

BUFSTACK_END_NAMESPACE
