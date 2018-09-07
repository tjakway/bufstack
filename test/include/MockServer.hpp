#pragma once

#include "Server.hpp"
#include "NamespaceDefines.hpp"

#include <memory>

BUFSTACK_BEGIN_NAMESPACE

class MockServer : public Server
{
public:
    MockServer() 
        : Loggable("MockServer"), Server()
    {}

    static void sendAll(int fd, const char* buf, ssize_t bufLen, Loggable& log)
    {
        Server::sendAll(fd, buf, bufLen, log);
    }

    void readFd(int fd, std::function<void(const std::vector<msgpack::object_handle>&)> callback)
    {
        Server::readFd(fd, callback);
    }


    virtual void onConnect(int /*clientFd*/) {}

    virtual void send(int, const char*, std::size_t) override {}

    virtual void send(int, Buffer) {} 
    virtual void waitUntilDone() {}
};

BUFSTACK_END_NAMESPACE
