#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include "MsgpackRpc.hpp"

#include <memory>
#include <mutex>
#include <deque>
#include <future>
#include <utility>
#include <atomic>
#include <fstream>
#include <vector>
#include <functional>
#include <chrono>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE

class Server : public virtual Loggable
{
private:
    std::atomic_bool done {false};
protected:
    const int backlogSize;
    const std::chrono::milliseconds sleepInterval;

    Server(int _backlogSize = Config::Defaults::defaultBacklogSize,
            std::chrono::milliseconds _sleepInterval = Config::Defaults::serverSleepInterval)
        : backlogSize(_backlogSize),
            sleepInterval(_sleepInterval)
    {}

    using Buffer = std::unique_ptr<std::pair<char*, long>>;

    virtual void onConnect(int clientFd) = 0;
    virtual Buffer onRecv(Buffer) = 0;
    virtual void send(int, Buffer) = 0;

    NEW_EXCEPTION_TYPE(ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(SocketError, ServerError);

    static void sendAll(int, const char* buf, ssize_t bufLen, Loggable&);
    virtual void readFd(int, std::function<void(const std::vector<msgpack::object_handle>&)>);


public:
    virtual ~Server() {}

    void startListening();

    void interrupt();
    bool interrupted();

    //basically call join()
    virtual void waitUntilDone() = 0;
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

class MsgpackServer : 
    public SingleConnectionServer, 
    public AsyncWriteServer
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(MsgpackServerError, ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotMessageError, MsgpackServerError);

    virtual void onReceiveResponseMsg(const MsgpackRpc::Message&) = 0;
    virtual void onReceiveRequestMsg(const MsgpackRpc::Message&) = 0;
    virtual void onReceiveNotificationMsg(const MsgpackRpc::Message&) = 0;

    virtual void onRecvMsg(const msgpack::object&);

private:
    msgpack::object_handle decode(Buffer);
};

BUFSTACK_END_NAMESPACE
