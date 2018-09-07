#pragma once

#include "NamespaceDefines.hpp"
#include "HasFd.hpp"
#include "Loggable.hpp"
#include "HasTcpConnection.hpp"

#include <cstdint>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

class ClientConnection : 
    virtual public HasClientFd,
    virtual public Loggable
{
protected:
    ClientConnection() {}

public:
    virtual ~ClientConnection() {}
};

class ClientTcpConnection 
    : public ClientConnection,
      public HasTcpConnection
{
public:
    ClientTcpConnection(
            const std::string& address,
            uint16_t port)
        : HasTcpConnection(address, port)
    {}

    virtual ~ClientTcpConnection() {}
};

BUFSTACK_END_NAMESPACE
