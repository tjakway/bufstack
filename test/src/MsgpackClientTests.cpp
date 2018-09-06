#include <gtest/gtest.h>

#include <limits>

#include <msgpack.hpp>

#include "Server.hpp"

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

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
        MsgpackClient(
            Server::localhost,
            std::numeric_limits<uint16_t>::max());
    } 
    catch(MsgpackClient::ConnectionError)
    {
        exceptionThrown = true;
    }

    ASSERT_TRUE(exceptionThrown);
}

BUFSTACK_END_NAMESPACE
