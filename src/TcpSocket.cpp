#include "TcpSocket.hpp"

#include "Util/Util.hpp"
#include "Util/Strcat.hpp"
#include "Util/CloseLogError.hpp"

#include <cstdlib>
#include <utility>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

BUFSTACK_BEGIN_NAMESPACE

std::pair<int,sockaddr_in> TcpSocket::newTcpSocket(
    const std::string& address, uint16_t port, Loggable& l)
{
    int sockFd = -1;
    try {
        sockFd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
        if(sockFd < 0)
        {
            auto _errno = errno;
            throw CreateSocketException(
                    STRCATS("Could not create socket (" <<
                        "socket returned " << sockFd << ").  " <<
                        "Error description: " << strerror(_errno)));
        }

        sockaddr_in sock;
        memset(&sock, 0, sizeof(sockaddr_in));
        sock.sin_family = AF_INET; //tcp
        sock.sin_port = htons(port);
        
        decodeAddress(address, &sock.sin_addr);
        if(inet_aton(address.c_str(), &sock.sin_addr) != 1)
        {
            throw BadAddressException(STRCATS(
                        "Could not interpret " <<
                        address << " as an IPv4" <<
                        " address"));
        }

        return std::make_pair(sockFd, sock);
    }
    catch(...)
    {
        //close the socket on error then rethrow
        SAFE_CLOSE_LOG_ERROR_LOGGABLE(sockFd, l);
        throw;
    }
}


void TcpSocket::decodeAddress(
        const std::string& address, in_addr* addr)
{
    if(Util::StringTrim::trim_copy(address).empty())
    {
        throw BadAddressException("Passed address is empty");
    }
    else if(addr == nullptr)
    {
        throw BadAddressException(
            STRCATS("in_addr* is null for call to " <<
                __func__  << " with address " << address));
    }
    else if(inet_aton(address.c_str(), addr) != 1)
    {
        throw BadAddressException(STRCATS(
                    "Could not interpret " <<
                    address << " as an IPv4" <<
                    " address"));
    }
}


BUFSTACK_END_NAMESPACE
