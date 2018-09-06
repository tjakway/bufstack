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
#include <string>
#include <utility>


BUFSTACK_BEGIN_NAMESPACE

void SingleConnectionServer::onConnect(int clientFd)
{
    if(connected.load())
    {
        throw SingleConnectionServerError("Already connected to one client");
    }
    else
    {
        connected.store(true);
    }
    Server::onConnect(clientFd);
}

AsyncWriteServer::AsyncWriteServer(
            int backlogSize,
            bool _forceAsync)
    : Server(backlogSize),
    forceAsync(_forceAsync)
{}

void AsyncWriteServer::send(int clientFd, Server::Buffer buf)
{
    //the function object to pass to std::async
    const auto doSendF = 
        std::bind(&AsyncWriteServer::doSend, this, 
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


void AsyncWriteServer::send(int clientFd, const char* buf, std::size_t len)
{
    Server::Buffer ptr = Server::Buffer(
            new Server::Buffer::element_type(
                std::make_pair(new char[len], len)));
    AsyncWriteServer::send(clientFd, std::move(ptr));
}


void AsyncWriteServer::doSend(int clientFd, Server::Buffer buf)
{
    std::unique_lock<std::mutex> {writeMutex};

    Server::sendAll(clientFd, buf->first, buf->second, *this);
}

BUFSTACK_END_NAMESPACE
