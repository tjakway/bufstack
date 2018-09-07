#pragma once

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"
#include "SingleConnectionServer.hpp"
#include "AsyncBufSender.hpp"

BUFSTACK_BEGIN_NAMESPACE

/**
 * class that receives msgpack RPC messages
 */
class MsgpackReceiver : 
    public SingleConnectionServer, 
    public AsyncBufSender,
    virtual public Loggable
{
    //handle*Message parse the message then pass it to the
    //appropriate callback
    void handleRequestMessage(const std::vector<msgpack::object>&);
    void handleResponseMessage(const std::vector<msgpack::object>&);
    void handleNotificationMessage(const std::vector<msgpack::object>&);

protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(MsgpackReceiverException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotMessageException, MsgpackReceiverException);

    virtual void onReceiveResponseMsg(const MsgpackRpc::ResponseMessage&) = 0;
    virtual void onReceiveRequestMsg(const MsgpackRpc::RequestMessage&) = 0;
    virtual void onReceiveNotificationMsg(const MsgpackRpc::NotificationMessage&) = 0;

    virtual void onRecvMsg(const msgpack::object&);

public:
    virtual ~MsgpackReceiver() {}
};

BUFSTACK_END_NAMESPACE
