#include "ClientConnection.hpp"

#include "Util/Strcat.hpp"
#include "Util/CloseLogError.hpp"
#include "Util/PrintableObject.hpp"
#include "TcpSocket.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <cstring>
#include <string>
#include <utility>

BUFSTACK_BEGIN_NAMESPACE

PrintableObject::Fields ClientTcpConnection::getFields() const noexcept
{
    return PrintableObject::Fields {
        std::make_pair("address", getAddress()),
        std::make_pair("port", std::to_string(getPort()))
    };
}

std::string ClientTcpConnection::getName() const noexcept
{
    return "ClientTcpConnection";
}

void ClientTcpConnection::_connect()
{
    getLogger()->set_level(spdlog::level::debug);

    //don't forget to call this!
    onConnect();

    int sockFd = -1;
    try {
        auto res = TcpSocket::newTcpSocket(getAddress(), getPort(), *this);
        sockFd = res.first;
        sockaddr_in sock = res.second;

        if(connect(sockFd, (sockaddr*)&sock, sizeof(sock)) < 0)
        {
            auto _errno = errno;
            throw ClientConnectionException(
                STRCATS("Call to connect(2) failed with error description: " <<
                    strerror(_errno)));
        }

        //connection successful, set fields
        setClientFd(sockFd);

        getLogger()->debug(STRCATS("TCP client connection successful on " <<
                    getAddress() << ":" << getPort()));
    }
    catch(...)
    {
        //close the socket on error then rethrow
        SAFE_CLOSE_LOG_ERROR(sockFd);
        throw;
    }
}

ClientTcpConnection::ClientTcpConnection(
        const std::string& address,
        uint16_t port)
    : Loggable(getName()),
    HasTcpConnection(address, port)
{
    _connect();
}

BUFSTACK_END_NAMESPACE
