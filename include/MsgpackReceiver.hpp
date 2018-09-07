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
    public AsyncBufSender
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(MsgpackBaseException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotMessageException, MsgpackBaseException);

    virtual void onReceiveResponseMsg(const MsgpackRpc::ResponseMessage&) = 0;
    virtual void onReceiveRequestMsg(const MsgpackRpc::RequestMessage&) = 0;
    virtual void onReceiveNotificationMsg(const MsgpackRpc::NotificationMessage&) = 0;

    virtual void onRecvMsg(const msgpack::object&);

private:
    msgpack::object_handle decode(Buffer);

public:
    virtual ~MsgpackReceiver() {}
};

BUFSTACK_END_NAMESPACE
