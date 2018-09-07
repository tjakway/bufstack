#include "SingleConnectionServer.hpp"

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

void SingleConnectionServer::onConnect(int clientFd)
{
    if(connected.load())
    {
        throw SingleConnectionServerError("Already connected to one client");
    }
    else
    {
        connected.store(true);
    }
    Server::onConnect(clientFd);
}

BUFSTACK_END_NAMESPACE
