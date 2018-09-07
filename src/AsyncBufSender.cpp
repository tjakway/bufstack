#include "AsyncBufSender.hpp"

#include "Buffer.hpp"

#include <utility>
#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

void AsyncBufSender::reapFutures() noexcept
{
    std::lock_guard<std::mutex> {writeMutex};

    futures.erase(std::remove_if(futures.begin(),
        futures.end(), [](const FutureType& f){
            return (!f.valid()) || 
            //see https://stackoverflow.com/questions/10890242/get-the-status-of-a-stdfuture
            //re: checking if a future is ready
                (f.wait_for(std::chrono::seconds(0)) == 
                    std::future_status::ready);
        }));
}

AsyncBufSender::AsyncBufSender(
            std::size_t _backlogSize,
            bool _forceAsync)
    : Loggable("AsyncBufSender"),
    backlogSize(_backlogSize),
    forceAsync(_forceAsync)
{}

void AsyncBufSender::send(int clientFd, Buffer buf)
{
    std::lock_guard<std::mutex> {writeMutex};

    //the function object to pass to std::async
    const auto doSendF = 
        std::bind(&AsyncBufSender::doSend, this, 
                std::placeholders::_1, std::placeholders::_2);

    std::future<void> result;
    //if forceAsync == true, force the use of a separate write thread
    if(forceAsync)
    {
        result = std::async(doSendF, clientFd, std::move(buf), std::launch::async);
    }
    else
    {
        //otherwise use default launch flags
        result = std::async(doSendF, clientFd, std::move(buf));
    }

    //***NEED*** to keep the returned std::future, otherwise its destructor
    //will block until the thread completes, making our code effectively synchronous
    //see https://stackoverflow.com/questions/36920579/how-to-use-stdasync-to-call-a-function-in-a-mutex-protected-loop
    futures.emplace_back(std::move(result));
}


void AsyncBufSender::send(int clientFd, const char* buf, std::size_t len)
{
    Buffer ptr = Buffer(
            new Buffer::element_type(
                std::make_pair(new char[len], len)));
    AsyncBufSender::send(clientFd, std::move(ptr));
}


void AsyncBufSender::doSend(int clientFd, Buffer buf)
{
    std::unique_lock<std::mutex> {writeMutex};

    BufSender::sendAll(clientFd, buf->first, buf->second, *this);
}

BUFSTACK_END_NAMESPACE
