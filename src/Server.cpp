#include "Server.hpp"

#include "NamespaceDefines.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

//low level socket write function with error checking
void Server::sendAll(int clientFd, char* buf, ssize_t bufLen)
{
    if(bufLen <= 0)
    {
        throw SocketError(
                STRCAT("Could not write to socket file descriptor ", 
                    clientFd, ": buffer length <= 0 (actual: ", bufLen, ")"));
    }
    else if(bufLen == std::numeric_limits<ssize_t>::max())
    {
        warn() << "bufLen == std::numeric_limits<ssize_t>::max()" << std::endl;
    }

    //don't write more than this at once
    auto maxWriteCycle = sizeof(size_t) - 1;
    char* currentBufPosition = buf;

    long remaining = bufLen;
    while(remaining > 0)
    {
        //try to write at most this amount
        //according to the man page:
        //"if count > SSIZE_MAX, the result is implementation-defined"
        //let's avoid that
        size_t amountToWrite = 
            min(
                    min(
                        static_cast<decltype(maxWriteCycle)>(remaining), 
                        maxWriteCycle), 
                    static_cast<decltype(maxWriteCycle)>(SSIZE_MAX));

        ssize_t amountWritten = write(clientFd, currentBufPosition, amountToWrite);

        if(amountWritten < 0)
        {
            switch(errno)
            {
                case EINTR:
                    warn() << "encountered EINTR, indicating the write call was "
                        << "interrupted by a signal before any data was rewritten"
                        << ".  Retrying..." << std::endl;
                    continue;
                default:
                    throw SocketError(STRCAT("Error in sendAll: ", 
                                strerror(errno)));
            }
        }
        else
        {
            remaining -= amountWritten;
            if(remaining < 0)
            {
                throw SocketError(
                        STRCAT("Error while writing to socket file descriptor ", 
                            clientFd, ": remaining < 0 (actual:", remaining, ")"));
            }

            currentBufPosition += amountWritten;
        }
    }
}

BUFSTACK_END_NAMESPACE
