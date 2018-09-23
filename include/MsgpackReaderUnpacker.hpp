#pragma once

#include <utility>
#include <functional>

#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "Interruptible.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "HasFd.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackReaderUnpacker : 
    virtual public Loggable,
    public Interruptible
{
    const std::size_t backlogSize;
    using SleepIntervalType = std::chrono::milliseconds;
    const SleepIntervalType sleepInterval;
public:
    using ObjectList = std::vector<std::reference_wrapper<const msgpack::object>>;
    using Callback = std::function<void(const ObjectList&)>;

protected:
    virtual void readFd(int, Callback);


public:
    NEW_EXCEPTION_TYPE(MsgpackReaderUnpackerException);
    NEW_EXCEPTION_TYPE_WITH_BASE(SelectException, MsgpackReaderUnpackerException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ReadException, MsgpackReaderUnpackerException);

    MsgpackReaderUnpacker();

    MsgpackReaderUnpacker(
            std::size_t _backlogSize,
            std::chrono::milliseconds _sleepInterval);

    virtual ~MsgpackReaderUnpacker();

    //alias for interrupt
    void done();

    virtual void startListening() = 0;
};

BUFSTACK_END_NAMESPACE
