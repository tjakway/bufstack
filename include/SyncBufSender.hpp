#pragma once

#include "NamespaceDefines.hpp"
#include "BufSender.hpp"
#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

class SyncBufSender : virtual public Loggable
{
public:
    virtual void send(int fd, Buffer buf) override
    {
        BufSender::sendAll(fd, buf->first, buf->second, *this);
    }

    virtual void send(int fd, const char* data, std::size_t len) override
    {
        BufSender::sendAll(fd, data, len, *this);
    }

    SyncBufSender()
        : Loggable("SyncBufSender")
    {}

    virtual ~SyncBufSender() {}
};

BUFSTACK_END_NAMESPACE
