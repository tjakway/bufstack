#pragma once

#include "NamespaceDefines.hpp"

#include "AsyncBufSender.hpp"

BUFSTACK_BEGIN_NAMESPACE

/**
 * implemented by calling the appropriate methods in 
 * AsyncBufSender then waiting until done
 */
class SyncBufSender : public AsyncBufSender
{
    void waitFutures()
    {
        for(FutureType& f : futures)
        {
            f.wait();
        }
    }

public:
    SyncBufSender(
        std::size_t backlogSize = Config::Defaults::defaultBacklogSize)
        : Loggable("SyncBufSender"),
        AsyncBufSender(backlogSize, false)  //forceAsync = false
    {}

    virtual ~SyncBufSender() {}

    virtual void send(int fd, Buffer b) override
    {
        AsyncBufSender::send(fd, b);
        waitFutures();
    }
    virtual void send(int fd, const char* buf, std::size_t len) override
    {
        AsyncBufSender::send(fd, buf, len);
        waitFutures();
    }
};

BUFSTACK_END_NAMESPACE
