#pragma once

#include "NamespaceDefines.hpp"
#include "Connectible.hpp"
#include "Util/NewExceptionType.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <string>

BUFSTACK_BEGIN_NAMESPACE

class HasTcpConnection : virtual public Connectible
{
    const std::string address;
    const uint16_t port;

protected:
    static void decodeAddress(const std::string&, in_addr*);

public:
    HasTcpConnection(
            const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port)
    {}

    virtual ~HasTcpConnection() {}

    std::string getAddress() const noexcept { return address; }
    uint16_t getPort() const noexcept { return port; }

    NEW_EXCEPTION_TYPE_WITH_BASE(BadAddressException, 
            BaseException);

    static constexpr const char localhost[] = "127.0.0.1";
};

BUFSTACK_END_NAMESPACE
