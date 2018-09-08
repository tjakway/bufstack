#pragma once

#include "NamespaceDefines.hpp"
#include "HasFd.hpp"
#include "Loggable.hpp"
#include "HasTcpConnection.hpp"
#include "ConnectionInfo.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/PrintableObject.hpp"

#include <cstdint>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

class ClientConnection : 
    virtual public Connectible,
    virtual public HasClientFd,
    virtual public Loggable,
    virtual public PrintableObject
{
protected:
    ClientConnection() {}
    virtual void onConnect() override;

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
    virtual void abstract() override {}
    void _connect();

protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;

public:
    ClientTcpConnection(
            const std::string& address,
            uint16_t port);
    ClientTcpConnection(const ClientTcpConnection&) = delete;
    ClientTcpConnection(ClientTcpConnection&&);

    virtual ~ClientTcpConnection() {}

};

/**
 * connection over unix domain sockets
 */
class ClientUnixConnection
    : public ClientConnection
{
    virtual void abstract() override {}

    void _connect();
    const std::string path;

protected:
    virtual Fields getFields() const noexcept override;
    virtual std::string getName() const noexcept override;
public:
    ClientUnixConnection(const std::string&);
    ClientUnixConnection(const ClientUnixConnection&) = delete;
    ClientUnixConnection(ClientUnixConnection&&);

    virtual ~ClientUnixConnection() {}

    std::string getPath() const noexcept { return path; }
};


BUFSTACK_END_NAMESPACE
