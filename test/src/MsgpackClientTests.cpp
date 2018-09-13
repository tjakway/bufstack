#include <gtest/gtest.h>

#include <limits>


#include "NamespaceDefines.hpp"
#include "Loggable.hpp"
#include "MockMsgpackClient.hpp"
#include "ClientConnection.hpp"
#include "NvimConnectionTest.hpp"

#include <msgpack.hpp>

BUFSTACK_BEGIN_NAMESPACE



class MsgpackClientTests 
    : public ::testing::Test, 
    public NvimConnectionTest,
    public Loggable
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
    //std::shared_ptr<MsgpackClient> inst = getClientInstance();
}

BUFSTACK_END_NAMESPACE
