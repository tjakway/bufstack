#include "SingleConnection.hpp"

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

void SingleConnection::onConnect()
{
    if(connected.load())
    {
        throw SingleConnectionException("Already connected to one client");
    }
    else
    {
        connected.store(true);
    }
}

BUFSTACK_END_NAMESPACE
