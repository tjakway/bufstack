#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <string>
#include <cstdint>

BUFSTACK_BEGIN_NAMESPACE

class TcpSocket
{
    TcpSocket() = delete;

public:
    //TODO: probably a better place to put these
    NEW_EXCEPTION_TYPE(CreateSocketException);
    NEW_EXCEPTION_TYPE(BadAddressException);

    static std::pair<int,sockaddr_in> newTcpSocket(
        const std::string& addr, uint16_t port, Loggable&);
    static void decodeAddress(const std::string&, in_addr*);
};

BUFSTACK_END_NAMESPACE
