#pragma once

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"
#include "Util/AtomicAccess.hpp"
#include "Util/AtomicSequence.hpp"
#include "HasFd.hpp"
#include "AsyncBufSender.hpp"
#include "MsgpackReceiver.hpp"

BUFSTACK_BEGIN_NAMESPACE

class AbstractMsgpackClient : 
    virtual public MsgpackReceiver,
    virtual public HasClientFd,
    public AsyncBufSender
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
    using BaseException = MsgpackReceiver::BaseException;
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseResultConversionException, ResponseException);
    //indicates a serverside error
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseGotException, ResponseException);

    virtual void addResponseCallback(BoundResponseCallback&& cb);
    virtual void onReceiveNotificationMsg(const MsgpackRpc::NotificationMessage&) override;

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
                    throw ResponseGotException(STRCATS(
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

        send(getClientFd(), buffer->data(), buffer->size());

        std::shared_ptr<std::promise<T>> thisPromise = 
            std::make_shared<std::promise<T>>();

        addResponseCallback(std::bind(thisMsgId, thisPromise,
                    std::placeholders::_1));

        return thisPromise->get_future();
    }

    AbstractMsgpackClient()
        : idSeq(0), responseCallbacks(
            std::vector<BoundResponseCallback>{})
    {}

    virtual ~AbstractMsgpackClient() {}
};

BUFSTACK_END_NAMESPACE
