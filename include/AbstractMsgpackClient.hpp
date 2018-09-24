#pragma once

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"
#include "Util/AtomicAccess.hpp"
#include "Util/AtomicSequence.hpp"
#include "Util/MsgpackUtil.hpp"
#include "HasFd.hpp"
#include "AsyncBufSender.hpp"
#include "MsgpackReceiver.hpp"
#include "MsgpackFdReader.hpp"
#include "ClientConnection.hpp"

#include <cstdlib>
#include <tuple>
#include <mutex>

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
class ConvertResponseValue<void>
{
public:
    void operator()(
        std::shared_ptr<std::promise<void>> promise,
        const msgpack::object& objectReceived)
    {
        promise->set_value();
    }
};

template <>
class ConvertResponseValue<msgpack::object_handle>
{
public:
    void operator()(
        std::shared_ptr<std::promise<msgpack::object_handle>> promise,
        const msgpack::object& objectReceived)
    {
        promise->set_value(MsgpackUtil::clone(objectReceived));
    }
};

class AbstractMsgpackClient : 
    virtual public MsgpackReceiver,
    public AsyncBufSender
{
    using MsgId = uint32_t;

    std::vector<std::pair<std::string, MsgId>> openCalls;
    AtomicSequence<MsgId> idSeq;


    template <typename T>
    using UnboundResponseCallback = std::function<bool(MsgId, 
            std::shared_ptr<std::promise<T>>,
            const MsgpackRpc::ResponseMessage&,
            MtLoggable&)>;

    using BoundResponseCallback = 
        std::function<bool(MsgpackRpc::ResponseMessage)>;

    AtomicAccess<std::vector<BoundResponseCallback>> responseCallbacks;

    std::mutex fdReaderMutex;
    std::unique_ptr<MsgpackFdReader> fdReader;
    void onDecodeCallback(const MsgpackReaderUnpacker::ObjectList&);

    void startListening(int readFd, std::function<void(MsgpackFdReader&)>);
    
protected:
    using BaseException = MsgpackReceiver::BaseException;
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseResultConversionException, ResponseException);
    //indicates a serverside error
    NEW_EXCEPTION_TYPE_WITH_BASE(ResponseGotException, ResponseException);

    NEW_EXCEPTION_TYPE_WITH_BASE(AlreadyListeningException, BaseException);


    std::recursive_mutex callMutex;

    void startListening(int readFd);
    void asyncStartListening(int readFd);

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

    virtual ClientConnection& getClientConnection() const = 0;

    /**
     * returns true if this callback is finished
     */
    template <typename T>
    static bool setResponseValue(MsgId thisCallId,
            std::shared_ptr<std::promise<T>> promise,
            const MsgpackRpc::ResponseMessage& responseMsg,
            MtLoggable& logger)
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
            catch(std::exception& e)
            {
                logger.getLogger()->warn(
                    STRCATS("Caught exception in " << __func__ <<
                        ": " << e.what()));
                promise->set_exception(std::current_exception());
            }
        }
        else
        {
            return false;
        }
    }

    template <typename... Args>
    MsgId sendCall(const std::string& name, Args... args)
    {
        std::lock_guard<decltype(callMutex)> {callMutex};
        const auto thisMsgId = idSeq.nextAndIncrement();

        auto call_obj =
            std::make_tuple(
                MsgpackRpc::Message::getTypeValue(
                    MsgpackRpc::Message::Type::Request), 
                static_cast<uint32_t>(thisMsgId), name, 
                std::forward_as_tuple(args...));

        std::stringstream buffer;
        RPCLIB_MSGPACK::pack(buffer, call_obj);

        std::string str(buffer.str());
        const char* data = str.data();
        const auto len = str.size();

        int writeFd = getClientConnection().getWriteFd();
        send(writeFd, data, len);

        return thisMsgId;
    }

public:

    //TODO: implement
    template <typename... Args>
    void asyncCallVoidReturn(const std::string& name, 
            Args... args);

    template <typename T, typename... Args>
    std::future<T> asyncCall(const std::string& name, Args... args)
    {
        std::lock_guard<std::recursive_mutex> {callMutex};

        MsgId thisMsgId = sendCall<Args...>(name, args...);

        std::shared_ptr<std::promise<T>> thisPromise = 
            std::make_shared<std::promise<T>>();


        BoundResponseCallback cb = 
            [this, thisMsgId, thisPromise](const MsgpackRpc::ResponseMessage& m)
            -> bool
        {
            return AbstractMsgpackClient::setResponseValue(
                    thisMsgId, thisPromise, m, 
                    //pass *this as the instance of Loggable
                    *this);
        };

        addResponseCallback(std::move(cb));


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
        call<void, Args...>(name, args...);
    }

    AbstractMsgpackClient()
        : idSeq(0), responseCallbacks(
            std::vector<BoundResponseCallback>{}),
        fdReader()
    {}

    virtual ~AbstractMsgpackClient();
};

BUFSTACK_END_NAMESPACE
