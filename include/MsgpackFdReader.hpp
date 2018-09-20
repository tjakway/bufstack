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

    int getFd() const { return fd; }

    virtual ~MsgpackFdReader() {}

    virtual void startListening() override;
    virtual void asyncStartListening();
};

BUFSTACK_END_NAMESPACE
