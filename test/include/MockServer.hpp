#pragma once

#include "Server.hpp"
#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockServer : public Server
{
public:
    static void sendAll(int fd, const char* buf, ssize_t bufLen, Loggable& log)
    {
        Server::sendAll(fd, buf, bufLen, log);
    }

    void readFd(int fd, std::function<void(const std::vector<msgpack::object_handle>&)> callback)
    {
        Server::readFd(fd, callback);
    }
};

BUFSTACK_END_NAMESPACE
