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

/**
 * we only expect one neovim instance to connect at a time
 */
class SingleConnectionServer : public Server
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(SingleConnectionServerError, ServerError);

    std::atomic_bool connected {false};
    virtual void onConnect(int clientFd) override;
};


class AsyncWriteServer : public Server
{
protected:
    AsyncWriteServer(
            std::size_t backlogSize = Config::Defaults::defaultBacklogSize,
            bool _forceAsync = false);
    virtual void send(int, Buffer) override;
    virtual void send(int, const char*, std::size_t) override;

    NEW_EXCEPTION_TYPE_WITH_BASE(AsyncWriteServerError, ServerError);

    void doSend(int, Buffer);
private:
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeMutex;

    //newest futures will be at the front of the queue
    std::deque<std::future<void>> futures;
};

class MsgpackServer : 
    public SingleConnectionServer, 
    public AsyncWriteServer
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(MsgpackServerError, ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotMessageError, MsgpackServerError);

    virtual void onReceiveResponseMsg(const MsgpackRpc::Message&) = 0;
    virtual void onReceiveRequestMsg(const MsgpackRpc::Message&) = 0;
    virtual void onReceiveNotificationMsg(const MsgpackRpc::Message&) = 0;

    virtual void onRecvMsg(const msgpack::object&);

private:
    msgpack::object_handle decode(Buffer);
};

class MsgpackServerClient : 
    virtual public MsgpackServer,
    virtual public HasClientFd
{
    using MsgId = uint32_t;

    std::vector<std::pair<std::string, MsgId>> openCalls;
    AtomicSequence<MsgId> idSeq;


    template <typename T>
    using UnboundResponseCallback = std::function<void(MsgId, 
            std::shared_ptr<std::promise<T>>,
            MsgpackRpc::ResponseMessage)>;

    using BoundResponseCallback = std::function<void(MsgId)>;

    AtomicAccess<std::vector<BoundResponseCallback>> responseCallbacks;
    
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseError, ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseResultConversionError, ResponseError);
    //indicates a serverside error
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseGotError, ResponseError);

    virtual void addResponseCallback(BoundResponseCallback&& cb);
    virtual void onReceiveNotificationMsg(const MsgpackRpc::Message&) override;

    template <typename T>
    static void setResponseValue(
            const MsgpackRpc::ResponseMessage& responseMsg,
            MsgId thisCallId,
            std::shared_ptr<std::promise<T>> promise)
    {
        //check if this is the response we're waiting for
        if(responseMsg.msgId == thisCallId)
        {
            const msgpack::object& objectReceived = 
                responseMsg.result->get();

            //try to convert the response to that type
            try {
                if(responseMsg.isError())
                {
                    throw ResponseGotError(STRCATS(
                        "Server returned an error for request with " <<
                        "MsgId < " << responseMsg.msgId << 
                        " >.  Object received: " <<
                        objectReceived));
                }

                try {
                    //creates a new object of T
                    //see https://github.com/msgpack/msgpack-c/issues/480
                    promise->set_value(objectReceived.as<T>());
                }
                catch(msgpack::type_error e)
                {
                    throw ResponseResultConversionError(
                        STRCATS("Could not convert request " <<
                            "response to the desired " <<
                            "type for MsgId < " << responseMsg.msgId << 
                            " >.  Object received: " << objectReceived));
                }
            }
            catch(...)
            {
                promise.set_exception(std::current_exception());
            }
        }
    }

public:

    template <typename... Args>
    void asyncCallVoidReturn(const std::string& name, 
            Args... args);

    template <typename T, typename... Args>
    std::future<T> asyncCall(const std::string& name, Args... args)
    {
        const auto thisMsgId = idSeq.nextAndIncrement();

        auto call_obj =
            std::make_tuple(static_cast<uint8_t>(
                        MsgpackRpc::Message::Type::Request), 
                    thisMsgId, name, args...);

        auto buffer = std::make_shared<RPCLIB_MSGPACK::sbuffer>();
        RPCLIB_MSGPACK::pack(*buffer, call_obj);

        AsyncWriteServer::send(getClientFd(), buffer->data(), buffer->size());

        std::shared_ptr<std::promise<T>> thisPromise = 
            std::make_shared<std::promise<T>>();

        addResponseCallback(std::bind(thisMsgId, thisPromise,
                    std::placeholders::_1));

        return thisPromise->get_future();
    }

    MsgpackServerClient()
        : idSeq(0), responseCallbacks(
                std::vector<BoundResponseCallback>{})
    {}
};


class MsgpackClient : 
    protected MsgpackServerClient,
    virtual public HasClientFd
{


public:
    MsgpackClient(
        const std::string& address,
        uint16_t port);


    NEW_EXCEPTION_TYPE_WITH_BASE(ConnectionError, ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(BadAddressException, ConnectionError);
};

BUFSTACK_END_NAMESPACE
