#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"

#include <memory>
#include <mutex>

BUFSTACK_BEGIN_NAMESPACE

class Server
{
protected:
    Server(int serverFd, 
            sockaddr_in server, 
            int backlogSize = Config::Defaults::defaultBacklogSize);

    virtual void onConnect(int clientFd) = 0;
    virtual std::unique_ptr<char*> onRecv(std::unique_ptr<char*>) = 0;
    virtual void send(std::unique_ptr<char*>) = 0;

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
    virtual void send(std::unique_ptr<char*>) override;

private:
    std::mutex writeLock;
    void doSend(std::unique_ptr<char*>);
};

class MsgpackServer : public SingleConnectionServer, public AsyncWriteServer
{
protected:
    virtual void onRecvMsg(msgpack::object_handle) = 0;

private:
    msgpack::object_handle decode(std::unique_ptr<char*>);
};

BUFSTACK_END_NAMESPACE
