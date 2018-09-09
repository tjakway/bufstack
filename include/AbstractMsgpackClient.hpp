#pragma once

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"
#include "Util/AtomicAccess.hpp"
#include "Util/AtomicSequence.hpp"
#include "Util/MsgpackUtil.hpp"
#include "HasFd.hpp"
#include "AsyncBufSender.hpp"
#include "MsgpackReceiver.hpp"

BUFSTACK_BEGIN_NAMESPACE

template <typename T>
class ConvertResponseValue
{
public:
    void operator()(
        std::shared_ptr<std::promise<T>> promise,
        const msgpack::object& objectReceived)
    {
        promise->set_value(objectReceived.as<T>());
    }
};

template <>
class ConvertResponseValue<msgpack::object_handle>
{
public:
    void operator()(
        std::shared_ptr<std::promise<object_handle>> promise,
        const msgpack::object& objectReceived)
    {
        promise->set_value(MsgpackUtil::clone(objectReceived));
    }
};

class AbstractMsgpackClient : 
    virtual public MsgpackReceiver,
    virtual public HasClientFd,
    public AsyncBufSender
{
    using MsgId = uint32_t;

    std::vector<std::pair<std::string, MsgId>> openCalls;
    AtomicSequence<MsgId> idSeq;


    template <typename T>
    using UnboundResponseCallback = std::function<bool(MsgId, 
            std::shared_ptr<std::promise<T>>,
            MsgpackRpc::ResponseMessage)>;

    using BoundResponseCallback = std::function<bool(MsgId)>;

    AtomicAccess<std::vector<BoundResponseCallback>> responseCallbacks;
    
protected:
    using BaseException = MsgpackReceiver::BaseException;
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseResultConversionException, ResponseException);
    //indicates a serverside error
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseGotException, ResponseException);

    virtual void addResponseCallback(BoundResponseCallback&& cb);
    virtual void onReceiveResponseMsg(
            const MsgpackRpc::ResponseMessage&) override;

    template <typename T>
    static void convertResponseValue(
            std::shared_ptr<std::promise<T>> promise,
            const msgpack::object& objectReceived)
    {
        ConvertResponseValue<T>()(promise, objectReceived);
    }

    /**
     * returns true if this callback is finished
     */
    template <typename T>
    static bool setResponseValue(
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
                    throw ResponseGotException(STRCATS(
                        "Server returned an error for request with " <<
                        "MsgId < " << responseMsg.msgId << 
                        " >.  Object received: " <<
                        objectReceived));
                }

                try {
                    //creates a new object of T
                    //see https://github.com/msgpack/msgpack-c/issues/480
                    convertResponseValue(promise, objectReceived);

                    //indicate that this callback is no longer needed
                    return true;
                }
                catch(msgpack::type_error e)
                {
                    throw ResponseResultConversionException(
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
        else
        {
            return false;
        }
    }

public:

    //TODO: implement
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

        send(getClientFd(), buffer->data(), buffer->size());

        std::shared_ptr<std::promise<T>> thisPromise = 
            std::make_shared<std::promise<T>>();

        addResponseCallback(std::bind(
            thisMsgId, thisPromise, std::placeholders::_1));


        return thisPromise->get_future();
    }

    template <typename T, typename... Args>
    T call(const std::string& name, Args... args)
    {
        return asyncCall<T, Args...>(name, args...).get();
    }


    template <typename... Args>
    void callVoidReturn(const std::string& name, Args... args)
    {
        call<void>(name, args...).get();
    }

    AbstractMsgpackClient()
        : idSeq(0), responseCallbacks(
            std::vector<BoundResponseCallback>{})
    {}

    virtual ~AbstractMsgpackClient() {}
};

BUFSTACK_END_NAMESPACE
