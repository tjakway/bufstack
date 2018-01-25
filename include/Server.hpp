#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"

#include <memory>
#include <mutex>
#include <deque>
#include <future>
#include <utility>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

BUFSTACK_BEGIN_NAMESPACE

class Server
{
protected:
    Server(int serverFd, 
            sockaddr_in server, 
            int backlogSize = Config::Defaults::defaultBacklogSize);

    using Buffer = std::unique_ptr<std::pair<char*, long>>;

    virtual void onConnect(int clientFd) = 0;
    virtual Buffer onRecv(Buffer) = 0;
    virtual void send(Buffer) = 0;

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
    virtual void onConnect(int clientFd) override;
};

class AsyncWriteServer : public Server
{
protected:
    AsyncWriteServer(bool _forceAsync);
    virtual void send(Buffer) override;

private:
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeLock;

    //newest futures will be at the front of the queue
    std::deque<std::future<void>> futures;
    void doSend(Buffer);
};

class MsgpackServer : public SingleConnectionServer, public AsyncWriteServer
{
protected:
    virtual void onRecvMsg(msgpack::object_handle) = 0;

private:
    msgpack::object_handle decode(Buffer);
};

BUFSTACK_END_NAMESPACE
