#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackClient 
    : public MsgpackClient
{
private:
    virtual void abstract() override {}

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
