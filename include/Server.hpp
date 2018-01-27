#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include <memory>
#include <mutex>
#include <deque>
#include <future>
#include <utility>
#include <atomic>
#include <fstream>
#include <vector>
#include <functional>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE

class Server : public Loggable
{
private:
    std::atomic_bool done {false};
protected:
    Server(int serverFd, 
            sockaddr_in server, 
            int backlogSize = Config::Defaults::defaultBacklogSize);

    using Buffer = std::unique_ptr<std::pair<char*, long>>;

    virtual void onConnect(int clientFd) = 0;
    virtual Buffer onRecv(Buffer) = 0;
    virtual void send(int, Buffer) = 0;

    NEW_EXCEPTION_TYPE(ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(SocketError, ServerError);

    static void sendAll(int, const char* buf, ssize_t bufLen, Loggable&);
    void readFd(int, std::function<void(const std::vector<msgpack::object_handle>&)>);

public:
    void startListening();

    //basically call join()
    virtual void waitUntilDone();
};

/**
 * we only expect one neovim instance to connect at a time
 */
class SingleConnectionServer : public Server
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(SingleConnectionServerError, ServerError);

    std::atomic_bool connected {false};
    virtual void onConnect(int clientFd) override;
};

class AsyncWriteServer : public Server
{
protected:
    AsyncWriteServer(
            int serverFd, 
            sockaddr_in server, 
            int backlogSize = Config::Defaults::defaultBacklogSize,
            bool _forceAsync = false);
    virtual void send(int, Buffer) override;

    NEW_EXCEPTION_TYPE_WITH_BASE(AsyncWriteServerError, ServerError);

private:
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeMutex;

    //newest futures will be at the front of the queue
    std::deque<std::future<void>> futures;
    void doSend(int, Buffer);
};

class MsgpackServer : public SingleConnectionServer, public AsyncWriteServer
{
protected:
    virtual void onRecvMsg(msgpack::object_handle) = 0;

private:
    msgpack::object_handle decode(Buffer);
};

BUFSTACK_END_NAMESPACE
