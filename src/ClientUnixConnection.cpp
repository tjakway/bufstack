#include "ClientConnection.hpp"

#include "Util/Strcat.hpp"
#include "Util/CloseLogError.hpp"

#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <arpa/inet.h>

#include <cstring>
#include <string>


//un.h no longer defines UNIX_PATH_MAX
//see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=233946
struct sockaddr_un sizecheck;
#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX sizeof(sizecheck.sun_path)
#endif


BUFSTACK_BEGIN_NAMESPACE

PrintableObject::Fields ClientUnixConnection::getFields() const noexcept
{
    return PrintableObject::Fields {
        std::make_pair("path", getPath())
    };
}

std::string ClientUnixConnection::getName() const noexcept
{
    return "ClientUnixConnection";
}

void ClientUnixConnection::_connect()
{
    onConnect();

    int sockFd = -1;
    try {
        sockFd = socket(AF_UNIX, SOCK_STREAM, 0);
        if(sockFd < 0)
        {
            auto _errno = errno;
            throw ClientConnectionException(
                    STRCATS("Could not create socket (" <<
                        "socket returned " << sockFd << ").  " <<
                        "Error description: " << strerror(_errno)));
        }

        sockaddr_un sock;
        memset(&sock, 0, sizeof(sockaddr_un));

        sock.sun_family = AF_UNIX;

        //make sure our path isn't too long
        //on linux UNIX_PATH_MAX is 108
        //see https://stackoverflow.com/questions/34829600/why-is-the-maximal-path-length-allowed-for-unix-sockets-on-linux-108
        //+1 to include null terminator
        if((path.length()+1) > UNIX_PATH_MAX)
        {
            throw ClientConnectionException(STRCATS(
                "Passed path < " << getPath() << " > exceeds " <<
                "UNIX_PATH_MAX for this system (UNIX_PATH_MAX=" <<
                UNIX_PATH_MAX << ")"));
        }
        else
        {
            const auto bytesToCopy = sizeof(char) * (path.length() + 1);
            memcpy(&sock.sun_path, path.c_str(), bytesToCopy);
        }
        

        if(connect(sockFd, (sockaddr*)&sock, sizeof(sock)) < 0)
        {
            auto _errno = errno;
            throw ClientConnectionException(
                STRCATS("Call to connect(2) failed with error description: " <<
                    strerror(_errno)));
        }

        //connection successful, set fields
        setClientFd(sockFd);

        getLogger()->debug(STRCATS("Unix domain client connection successful on " <<
                    getPath()));
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
    : Loggable(getName()), path(_path)
{
    _connect();
}



BUFSTACK_END_NAMESPACE
