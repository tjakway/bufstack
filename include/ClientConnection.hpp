#pragma once

#include "NamespaceDefines.hpp"
#include "HasFd.hpp"
#include "Loggable.hpp"
#include "HasTcpConnection.hpp"
#include "ConnectionInfo.hpp"
#include "Util/NewExceptionType.hpp"

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

    NEW_EXCEPTION_TYPE(ClientConnectionException);

    static std::unique_ptr<ClientConnection> 
        newClientConnection(ConnectionInfo);
};

class ClientTcpConnection 
    : public ClientConnection,
      public HasTcpConnection
{
    void _connect();
public:
    ClientTcpConnection(
            const std::string& address,
            uint16_t port);

    virtual ~ClientTcpConnection() {}
};

/**
 * connection over unix domain sockets
 */
class ClientUnixConnection
    : public ClientConnection
{
    void _connect();
    const std::string path;
public:
    ClientUnixConnection(const std::string&);
    virtual ~ClientUnixConnection() {}

    std::string getPath() const noexcept { return path; }
};


BUFSTACK_END_NAMESPACE
