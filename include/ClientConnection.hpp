#pragma once

#include "NamespaceDefines.hpp"
#include "HasFd.hpp"
#include "Loggable.hpp"

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


    NEW_EXCEPTION_TYPE_WITH_BASE(ConnectionException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(BadAddressException, ConnectionException);
};

class ClientTcpConnection : public ClientConnection
{
    const std::string address;
    const uint16_t port;
    
public:
    ClientTcpConnection(
            const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port)
    {}

    virtual ~ClientTcpConnection() {}
};

BUFSTACK_END_NAMESPACE
