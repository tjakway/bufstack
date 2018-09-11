#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackClient.hpp"
#include "Loggable.hpp"
#include "ConnectionInfo.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackClient 
    : public MsgpackClient
{
private:
    virtual void onReceiveRequestMsg(
            const MsgpackRpc::RequestMessage&) override {}
    virtual void onReceiveNotificationMsg(
            const MsgpackRpc::NotificationMessage&) override {}
public:
    MockMsgpackClient(ConnectionInfo i)
        : Loggable("MockMsgpackClient"), MsgpackClient(i)
    {}
};

BUFSTACK_END_NAMESPACE
