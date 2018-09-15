#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackReaderUnpacker.hpp"
#include "Loggable.hpp"

#include <cstddef>
#include <chrono>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackFdReader
    : public MsgpackReaderUnpacker,
    virtual public Loggable
{
    const int fd;
    Callback cb;

public:
    MsgpackFdReader(
            int _fd,
            Callback onDecode,
            std::size_t _backlogSize,
            std::chrono::milliseconds _sleepInterval);

    virtual ~MsgpackFdReader() {}

    virtual void startListening() override;
};

BUFSTACK_END_NAMESPACE
