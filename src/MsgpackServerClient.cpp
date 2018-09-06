#include "Server.hpp"

#include <functional>


#include <sys/socket.h>
#include <arpa/inet.h> 
#include <unistd.h>    

#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE


void MsgpackServerClient::addResponseCallback(BoundResponseCallback&& cb)
{
    std::function<void(std::vector<BoundResponseCallback>&, 
            BoundResponseCallback&&)> f = 
        [](std::vector<BoundResponseCallback>& v, 
            BoundResponseCallback&& callback) -> void
    {
        v.emplace_back(callback);
    };

    auto g = [f, &cb](std::vector<BoundResponseCallback>& v){
        return f(v, std::move(cb));
    };
    responseCallbacks.access<void>(g);
}

MsgpackClient::MsgpackClient(
        const std::string& address,
        uint16_t port)
{
    int sockFd = -1;
    try {
        sockFd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
        if(sockFd < 0)
        {
            auto _errno = errno;
            throw ConnectionError(
                    STRCATS("Could not create socket (" <<
                        "socket returned " << sockFd << ").  " <<
                        "Error description: " << strerror(_errno)));
        }

        sockaddr_in sock {};
        sock.sin_family = AF_INET; //tcp
        sock.sin_port = htons(port);
        
        if(inet_aton(address.c_str(), &sock.sin_addr) != 1)
        {
            throw BadAddressException(STRCATS(
                        "Could not interpret " <<
                        address << " as an IPv4" <<
                        " address"));
        }


        if(connect(sockFd, (sockaddr*)&sock, sizeof(sock)) < 0)
        {
            auto _errno = errno;
            throw ConnectionError(
                    STRCATS("Call to connect(2) failed with error description: " <<
                        strerror(_errno)));
        }

        //connection successful, set fields
        setClientFd(sockFd);
    }
    catch(...)
    {
        //close the socket on error then rethrow
        if(sockFd > -1)
        {
            close(sockFd);
        }
        throw;
    }
}

BUFSTACK_END_NAMESPACE
