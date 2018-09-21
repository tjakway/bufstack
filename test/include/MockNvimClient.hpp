#pragma once

#include "NamespaceDefines.hpp"
#include "MsgpackClient.hpp"
#include "Loggable.hpp"
#include "ConnectionInfo.hpp"
#include "Util/Strcat.hpp"
#include "NvimClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockNvimClient
    : public NvimClient
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
    MockNvimClient(std::shared_ptr<ClientConnection> c)
        : Loggable("MockNvimClient"), NvimClient(c, false, true)
    {}

    MockNvimClient(ConnectionInfo i)
        : Loggable("MockNvimClient"), NvimClient(i)
    {}
};

BUFSTACK_END_NAMESPACE
