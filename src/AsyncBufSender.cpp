#include "AsyncBufSender.hpp"

#include "Buffer.hpp"

#include <utility>
#include <algorithm>
#include <iterator>

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

void AsyncBufSender::doSend(std::function<void(void)> writeF)
{
    std::lock_guard<std::mutex> {writeMutex};


    std::future<void> result;
    //if forceAsync == true, force the use of a separate write thread
    if(forceAsync)
    {
        result = std::async(std::launch::async, writeF);
    }
    else
    {
        //otherwise use default launch flags
        result = std::async(writeF);
    }

    //***NEED*** to keep the returned std::future, otherwise its destructor
    //will block until the thread completes, making our code effectively synchronous
    //see https://stackoverflow.com/questions/36920579/how-to-use-stdasync-to-call-a-function-in-a-mutex-protected-loop
    futures.emplace_back(std::move(result));
}


void AsyncBufSender::send(int clientFd, const char* buf, std::size_t len)
{
    //copy the data before sending so we know it won't be free'd before we're done
    std::vector<char> vec;
    vec.reserve(len);
    std::copy(buf, buf + len, std::back_inserter(vec));

    assert(memcmp(buf, vec.data(), len) == 0);

    auto sendF = [clientFd](std::vector<char> v, MtLoggable& log) {
        BufSender::sendAll(clientFd, v.data(), v.size(), log);
    };
    MtLoggable& ref = *this;
    std::function<void(void)> writeF = 
        std::bind(std::move(sendF), std::move(vec), ref);

    //bind *this for Loggable
    doSend(std::move(writeF));
}

void AsyncBufSender::send(int clientFd, Buffer buf)
{
    //move the buffer to the write function so we 
    //have ownership of the data when we write it
    auto f = [clientFd, this](Buffer mbuf) {
        BufSender::sendAll(clientFd, mbuf.data(), mbuf.size(), *this);
    };
    auto g = std::bind(f, std::move(buf));
    doSend(g);
}

BUFSTACK_END_NAMESPACE
