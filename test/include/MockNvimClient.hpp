#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackClient.hpp"
#include "Loggable.hpp"
#include "ConnectionInfo.hpp"
#include "Util/Strcat.hpp"
#include "MockMsgpackClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockNvimClient
    : public MockMsgpackClient
{
private:
    virtual void onReceiveRequestMsg(
            const MsgpackRpc::RequestMessage& msg) override 
    {
        getLogger()->debug(STRCATS("Received RequestMessage: " << msg.printCompact()));
    }

    virtual void onReceiveNotificationMsg(
            const MsgpackRpc::NotificationMessage& msg) override
    {
        getLogger()->debug(STRCATS("Received NotificationMessage: " << msg.printCompact()));
    }
    
public:
    MockNvimClient(ConnectionInfo i)
        : Loggable("MockNvimClient"), MockMsgpackClient(i)
    {}
};

BUFSTACK_END_NAMESPACE
