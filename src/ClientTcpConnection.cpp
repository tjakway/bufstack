#include "ClientConnection.hpp"

#include "Util/Strcat.hpp"
#include "Util/CloseLogError.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <string>

BUFSTACK_BEGIN_NAMESPACE

void ClientTcpConnection::connect()
{
    //don't forget to call this!
    onConnect();

    int sockFd = -1;
    try {
        sockFd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
        if(sockFd < 0)
        {
            auto _errno = errno;
            throw ConnectionException(
                    STRCATS("Could not create socket (" <<
                        "socket returned " << sockFd << ").  " <<
                        "Error description: " << strerror(_errno)));
        }

        sockaddr_in sock {};
        sock.sin_family = AF_INET; //tcp
        sock.sin_port = htons(getPort());
        
        decodeAddress(getAddress(), &sock.sin_addr);
        if(inet_aton(getAddress().c_str(), &sock.sin_addr) != 1)
        {
            throw BadAddressException(STRCATS(
                        "Could not interpret " <<
                        getAddress() << " as an IPv4" <<
                        " address"));
        }


        if(connect(sockFd, (sockaddr*)&sock, sizeof(sock)) < 0)
        {
            auto _errno = errno;
            throw ConnectionException(
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
    : HasTcpConnection(address, port)
{
    connect();
}

BUFSTACK_END_NAMESPACE
