#include "ClientConnection.hpp"

#include "Util/Strcat.hpp"
#include "Util/CloseLogError.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <arpa/inet.h>

#include <cstring>
#include <string>

BUFSTACK_BEGIN_NAMESPACE

void ClientUnixConnection::connect()
{
    onConnect();

    int sockFd = -1;
    try {
        sockFd = socket(AF_UNIX, SOCK_STREAM, 0);
        if(sockFd < 0)
        {
            auto _errno = errno;
            throw ConnectionException(
                    STRCATS("Could not create socket (" <<
                        "socket returned " << sockFd << ").  " <<
                        "Error description: " << strerror(_errno)));
        }

        sockaddr_un sock;
        memset(&sock, 0, sizeof(sockaddr_un));

        sock.sun_family = AF_UNIX;
        sock.sun_path = path.c_str();
        

        if(connect(sockFd, (sockaddr*)&sock, sizeof(sock)) < 0)
        {
            auto _errno = errno;
            throw ConnectionException(
                STRCATS("Call to connect(2) failed with error description: " <<
                    strerror(_errno)));
        }

        //connection successful, set fields
        setClientFd(sockFd);

        getLogger()->debug(STRCATS("Unix domain client connection successful on " <<
                    getPath());
    }
    catch(...)
    {
        //close the socket on error then rethrow
        SAFE_CLOSE_LOG_ERROR(sockFd);
        throw;
    }

}


ClientUnixConnection::ClientUnixConnection(
        const std::string& _path)
    : path(_path)
{
    _connect();
}



BUFSTACK_END_NAMESPACE
