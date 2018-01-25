#include "Server.hpp"

#include "NamespaceDefines.hpp"
#include "Util/Util.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Strcat.hpp"

#include <memory>
#include <functional>
#include <mutex>
#include <future>
#include <vector>
#include <sstream>
#include <limits>


BUFSTACK_BEGIN_NAMESPACE

AsyncWriteServer::AsyncWriteServer(
            int serverFd, 
            sockaddr_in server, 
            int backlogSize,
            bool _forceAsync)
    : Server(serverFd, server, backlogSize),
    forceAsync(_forceAsync)
{}

void AsyncWriteServer::send(Server::Buffer buf)
{
    //the function object to pass to std::async
    const auto doSendF = std::bind(&AsyncWriteServer::doSend, this, std::placeholders::_1);

    std::future<void> result;
    //if forceAsync == true, force the use of a separate write thread
    if(forceAsync)
    {
        result = std::async(doSendF, std::move(buf), std::launch::async);
    }
    else
    {
        //otherwise use default launch flags
        result = std::async(doSendF, std::move(buf));
    }

    //***NEED*** to keep the returned std::future, otherwise its destructor
    //will block until the thread completes, making our code effectively synchronous
    //see https://stackoverflow.com/questions/36920579/how-to-use-stdasync-to-call-a-function-in-a-mutex-protected-loop
    futures.emplace_back(std::move(result));
}


void AsyncWriteServer::doSend(Server::Buffer buf)
{
    std::unique_lock<std::mutex> {writeMutex};



}


//low level socket write function with error checking
void AsyncWriteServer::sendAll(int sockFd, char* buf, ssize_t bufLen)
{
    if(bufLen <= 0)
    {
        throw AsyncWriteServerError(
                STRCAT("Could not write to socket file descriptor ", 
                    sockFd, ": buffer length <= 0 (actual: ", bufLen, ")"));
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

        ssize_t amountWritten = write(sockFd, currentBufPosition, amountToWrite);

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
                    throw AsyncWriteServer(
                            STRCAT("Error in sendAll: ", 
                                strerror(errno)));
            }
        }
        else
        {
            //check for an overflow
            remaining -= amountWritten;
            if(remaining < 0)
            {
                throw AsyncWriteServerError(
                        STRCAT("Error while writing to socket file descriptor ", 
                            sockFd, ": remaining < 0 (actual:", remaining, ")");
            }

            currentBufPosition += amountWritten;
        }
    }
}

BUFSTACK_END_NAMESPACE
