#pragma once


#include "NamespaceDefines.hpp"

#include "Loggable.hpp"
#include "MsgpackReaderUnpacker.hpp"

#include <functional>
#include <vector>
#include <memory>
#include <stdexcept>

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackReaderUnpacker 
    : public MsgpackReaderUnpacker,
    virtual public Loggable
{
public:
    MockMsgpackReaderUnpacker() 
        : Loggable("MsgpackReaderUnpacker"), 
          MsgpackReaderUnpacker()
    {}

    virtual ~MockMsgpackReaderUnpacker() {}

    //expose protected method for testing
    void readFd(int fd, 
            std::function<void(const ObjectList&)> callback)
    {
        MsgpackReaderUnpacker::readFd(fd, callback);
    }

    virtual void startListening()
    {
        throw std::runtime_error("Not implemented");
    }

    virtual void abstract() {}
};

BUFSTACK_END_NAMESPACE
