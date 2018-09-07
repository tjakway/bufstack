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
public:
    class BufDeleter
    {
    public:
        void operator()(std::pair<char*, std::size_t>* buf)
        {
            delete[] buf->first;
            delete buf;
        }
    };
    using Buffer = std::unique_ptr<std::pair<char*, std::size_t>, BufDeleter>;
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


    virtual Buffer onRecv(Buffer) = 0;

private:
    NEW_EXCEPTION_TYPE(ServerException);
protected:
    //the base exception type should be accessed through this typedef
    using BaseException = ServerException;
    NEW_EXCEPTION_TYPE_WITH_BASE(SocketException, BaseException);

    virtual void readFd(int, std::function<void(const std::vector<msgpack::object_handle>&)>);

    virtual void onConnect(int clientFd) = 0;

public:
    virtual ~Server() {}

    void startListening();

    //basically call join()
    virtual void waitUntilDone() = 0;

    static constexpr auto localhost = "127.0.0.1";
};


class MsgpackClient : 
    protected MsgpackServerClient,
    virtual public HasClientFd
{


public:
    MsgpackClient(
        const std::string& address,
        uint16_t port);


    NEW_EXCEPTION_TYPE_WITH_BASE(ConnectionException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(BadAddressException, ConnectionException);
};

BUFSTACK_END_NAMESPACE
