#include <gtest/gtest.h>

#include <limits>

#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "MsgpackClient.hpp"
#include "ClientConnection.hpp"
#include "NvimConnectionTest.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MockMsgpackClient 
    : public MsgpackClient,
    public NvimConnectionTest
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


class MsgpackClientTests : public ::testing::Test, public Loggable
{
public:
    MsgpackClientTests()
        : Loggable("MsgpackClientTests")
    {}
};

TEST_F(MsgpackClientTests, TestBadPort)
{
    bool exceptionThrown = false;
    //connecting to an out of range port should throw an exception
    try {
        MockMsgpackClient(
            ConnectionInfo::tcpConnection(HasTcpConnection::localhost,
            std::numeric_limits<uint16_t>::max()));
    } 
    catch(ClientConnection::ClientConnectionException)
    {
        exceptionThrown = true;
    }

    ASSERT_TRUE(exceptionThrown);
}

TEST_F(MsgpackClientTests, TestLaunchNeovim)
{
    Client inst = getClientInstance();
}

BUFSTACK_END_NAMESPACE
