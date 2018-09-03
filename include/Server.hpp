#pragma once

#include "NamespaceDefines.hpp"
#include "Config.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/AtomicSequence.hpp"
#include "Util/AtomicAccess.hpp"
#include "Loggable.hpp"

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

class Server : public virtual Loggable
{
private:
    std::atomic_bool done {false};
protected:
    const int backlogSize;
    const std::chrono::milliseconds sleepInterval;

    Server(int _backlogSize = Config::Defaults::defaultBacklogSize,
            std::chrono::milliseconds _sleepInterval = Config::Defaults::serverSleepInterval)
        : backlogSize(_backlogSize),
            sleepInterval(_sleepInterval)
    {}

    using Buffer = std::unique_ptr<std::pair<char*, long>>;

    virtual void onConnect(int clientFd) = 0;
    virtual Buffer onRecv(Buffer) = 0;
    virtual void send(int, Buffer) = 0;

    NEW_EXCEPTION_TYPE(ServerError);
    NEW_EXCEPTION_TYPE_WITH_BASE(SocketError, ServerError);

    static void sendAll(int, const char* buf, ssize_t bufLen, Loggable&);
    virtual void readFd(int, std::function<void(const std::vector<msgpack::object_handle>&)>);


public:
    virtual ~Server() {}

    void startListening();

    void interrupt();
    bool interrupted();

    //basically call join()
    virtual void waitUntilDone() = 0;
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
            int serverFd, 
            sockaddr_in server, 
            int backlogSize = Config::Defaults::defaultBacklogSize,
            bool _forceAsync = false);
    virtual void send(int, Buffer) override;

    NEW_EXCEPTION_TYPE_WITH_BASE(AsyncWriteServerError, ServerError);

private:
    /** determines whether or not to pass std::launch::async */
    bool forceAsync;

    std::mutex writeMutex;

    //newest futures will be at the front of the queue
    std::deque<std::future<void>> futures;
    void doSend(int, Buffer);
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

class MsgpackServerClient : virtual public MsgpackServer
{
    using MsgId = uint32_t;

    std::vector<std::pair<std::string, MsgId>> openCalls;
    AtomicSequence<MsgId> idSeq;


    using UnboundResponseCallback = std::function<void(MsgId, 
            std::shared_ptr<std::promise>)>;
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
            MsgId thisCallId,
            std::shared_ptr<std::promise<T>> promise,
            const MsgpackRpc::ResponseMessage& responseMsg)
    {
        //check if this is the response we're waiting for
        if(responseMsg.msgId == thisCallId)
        {
            const msgpack::object& objectReceived = 
                responseMsg.result.get();

            //try to convert the response to that type
            try {
                if(responseMsg.error())
                {
                    throw ResponseGotError(STRCATS(
                        "Server returned an error for request with " <<
                        "MsgId < " << msgId << " >.  Object received: " <<
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
                            "type for MsgId < " << msgId << 
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

    template <typename Args...>
    virtual void asyncCallVoidReturn(const std::string& name, 
            Args... args);

    template <typename T, typename Args...>
    virtual std::future<T> asyncCall(const std::string& name, Args... args)
    {
        const auto id = idSeq.nextAndIncrement();

        //auto call_obj =
        //    std::make_tuple(static_cast<uint8_t>(client::request_type::call), idx,
        //                  func_name, args_obj);

        auto buffer = std::make_shared<RPCLIB_MSGPACK::sbuffer>();
        RPCLIB_MSGPACK::pack(*buffer, call_obj);



    }

    Client()
        : idSeq(0)
    {}
};

BUFSTACK_END_NAMESPACE
