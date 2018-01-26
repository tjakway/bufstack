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

    static std::vector<char> readFd(int fd)
    {
        return Server::readFd(fd);
    }
};

BUFSTACK_END_NAMESPACE
