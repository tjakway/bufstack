#pragma once

#include <utility>
#include <msgpack.hpp>

#include "Interruptible.hpp"
#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "HasFd.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackReaderUnpacker : 
    virtual public HasClientFd,
    public Interruptible
{
    const std::size_t backlogSize;
    const std::chrono::milliseconds sleepInterval;
public:
    using ObjectQueue = std::deque<std::reference_wrapper<msgpack::object>>;
    using ObjectList = std::vector<std::reference_wrapper<msgpack::object>>;

protected:
    virtual void readFd(int, 
            std::function<void(const ObjectList&)>);


public:
    MsgpackReaderUnpacker();

    MsgpackReaderUnpacker(
            std::size_t _backlogSize,
            std::chrono::milliseconds _sleepInterval);

    virtual ~MsgpackReaderUnpacker();

    //alias for interrupt
    void done();

    void startListening();

    //basically call join()
    virtual void waitUntilDone() = 0;
};

BUFSTACK_END_NAMESPACE
