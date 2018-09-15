#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

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
            const std::string& addr, uint16_t port);
    static void decodeAddress(const std::string&, in_addr*);
};

BUFSTACK_END_NAMESPACE
