#include "Server.hpp"

#include "NamespaceDefines.hpp"
#include "Util.hpp"

#include <memory>
#include <functional>
#include <mutex>
#include <future>
#include <vector>
#include <sstream>

namespace {
    void sendAll(int sockFd, char* buf, ssize_t bufLen)
    {
        if(bufLen <= 0)
        {
            //TODO: error
        }

        //don't write more than this at once
        const auto maxWriteCycle = sizeof(size_t) - 1;
        char* currentBufPosition = buf;

        long remaining = bufLen;
        while(remaining > 0)
        {
            //try to write at most this amount
            //according to the man page:
            //"if count > SSIZE_MAX, the result is implementation-defined"
            //let's avoid that
            size_t amountToWrite = min(min(remaining, maxWriteCycle), SSIZE_MAX);

            ssize_t amountWritten = write(sockFd, currentBufPosition, amountToWrite);

            if(amountWritten < 0)
            {
                //error occurred
            }
            else
            {
                //check for an overflow
                remaining -= amountWritten;
                if(remaining < 0)
                {
                    //TODO: error
                }

                currentBufPosition += amountWritten;
            }
        }
    }
}


BUFSTACK_BEGIN_NAMESPACE

AsyncWriteServer::AsyncWriteServer(bool _forceAsync)
    : forceAsync(_forceAsync)
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

BUFSTACK_END_NAMESPACE
