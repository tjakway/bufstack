#pragma once


#include "NamespaceDefines.hpp"

#include <memory>

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackReaderUnpacker : public MsgpackReaderUnpacker
{
public:
    MsgpackReaderUnpacker() 
        : Loggable("MsgpackReaderUnpacker"), 
          MsgpackReaderUnpacker()
    {}

    void readFd(int fd, std::function<void(const std::vector<msgpack::object_handle>&)> callback)
    {
        MsgpackReaderUnpacker::readFd(fd, callback);
    }

    virtual void waitUntilDone() {}
};

BUFSTACK_END_NAMESPACE
