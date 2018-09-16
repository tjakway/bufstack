#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackClient.hpp"
#include "Loggable.hpp"
#include "ConnectionInfo.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackClient 
    : public MsgpackClient
{
private:
    virtual void onReceiveRequestMsg(
            const MsgpackRpc::RequestMessage& msg) override 
    {
        getLogger()->debug(STRCATS("Received RequestMessage: " << msg));
    }

    virtual void onReceiveNotificationMsg(
            const MsgpackRpc::NotificationMessage&) override
    {
        getLogger()->debug(STRCATS("Received NotificationMessage: " << msg));
    }
    
public:
    MockMsgpackClient(ConnectionInfo i)
        : Loggable("MockMsgpackClient"), MsgpackClient(i)
    {}
};

BUFSTACK_END_NAMESPACE
