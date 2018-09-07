#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Strcat.hpp"
#include "Util/AtomicSequence.hpp"
#include "Util/AtomicAccess.hpp"
#include "Loggable.hpp"
#include "HasFd.hpp"
#include "Interruptible.hpp"

#include "MsgpackRpc.hpp"

#include <memory>
#include <mutex>
#include <deque>
#include <future>
#include <utility>
#include <atomic>
#include <fstream>
#include <vector>
#include <functional>
#include <chrono>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE

class Server : public virtual Loggable, public virtual Interruptible
{
private:
    std::atomic_bool done {false};
protected:
    const std::size_t backlogSize;
    const std::chrono::milliseconds sleepInterval;

    Server(std::size_t _backlogSize = Config::Defaults::defaultBacklogSize,
            std::chrono::milliseconds _sleepInterval = Config::Defaults::serverSleepInterval)
        : backlogSize(_backlogSize),
            sleepInterval(_sleepInterval)
    {}

private:
    NEW_EXCEPTION_TYPE(ServerException);
protected:
    //the base exception type should be accessed through this typedef
    using BaseException = ServerException;
    NEW_EXCEPTION_TYPE_WITH_BASE(SocketException, BaseException);

public:
    virtual ~Server() {}

    void startListening();

    //basically call join()
    virtual void waitUntilDone() = 0;
};



BUFSTACK_END_NAMESPACE
