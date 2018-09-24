#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackReaderUnpacker.hpp"
#include "Loggable.hpp"

#include <cstddef>
#include <chrono>
#include <memory>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackFdReader
    : public MsgpackReaderUnpacker,
    virtual public Loggable
{
    const int fd;
    Callback cb;
    std::atomic_bool listening;
    static const std::chrono::milliseconds spinInterval;

    std::unique_ptr<std::thread> listenThread;

public:
    MsgpackFdReader(
            int _fd,
            Callback onDecode,
            std::size_t _backlogSize,
            std::chrono::milliseconds _sleepInterval);

    MsgpackFdReader(
            int _fd,
            Callback onDecode);

    NEW_EXCEPTION_TYPE(SetFdNonblockingException);

    int getFd() const { return fd; }

    virtual ~MsgpackFdReader();

    virtual void startListening() override;
    virtual void asyncStartListening();

    bool isListening() const
    {
        return listening.load();
    }

    void join()
    {
        done();
        if(listenThread)
        {
            if(listenThread->joinable())
            {
                listenThread->join();
            }
        }
    }
};

BUFSTACK_END_NAMESPACE
