#pragma once

#include "BufSender.hpp"

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class AsyncBufSender : public BufSender
{
protected:
    AsyncBufSender(
            std::size_t backlogSize = Config::Defaults::defaultBacklogSize,
            bool _forceAsync = false);
    virtual void send(int, Buffer) override;
    virtual void send(int, const char*, std::size_t) override;

    NEW_EXCEPTION_TYPE_WITH_BASE(AsyncBufSenderError, ServerError);

    void doSend(int, Buffer);
private:
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeMutex;

    //newest futures will be at the front of the queue
    std::deque<std::future<void>> futures;

public:
    virtual ~AsyncBufSender() {}
};

BUFSTACK_END_NAMESPACE
