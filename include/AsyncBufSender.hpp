#pragma once

#include "Config.hpp"
#include "NamespaceDefines.hpp"
#include "BufSender.hpp"
#include "Buffer.hpp"
#include "Interruptible.hpp"

#include <deque>
#include <future>
#include <mutex>

BUFSTACK_BEGIN_NAMESPACE

class AsyncBufSender 
    : public BufSender, 
    public Interruptible,
    virtual public Loggable
{
protected:
    AsyncBufSender(
            std::size_t backlogSize = Config::Defaults::defaultBacklogSize,
            bool _forceAsync = false);
    virtual void send(int, Buffer) override;
    virtual void send(int, const char*, std::size_t) override;


    //newest futures will be at the front of the queue
    using FutureType = std::future<void>;
    std::deque<FutureType> futures;
    void reapFutures() noexcept;

    NEW_EXCEPTION_TYPE_WITH_BASE(AsyncBufSenderException, BaseException);

private:
    std::size_t backlogSize;
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeMutex;

    void doSend(int, Buffer);
public:
    virtual ~AsyncBufSender() {}
};

BUFSTACK_END_NAMESPACE
