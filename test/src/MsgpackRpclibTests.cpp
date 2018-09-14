#include <gtest/gtest.h>

#include "MsgpackClient.hpp"
#include "MockMsgpackClient.hpp"
#include "TestConfig.hpp"
#include "Loggable.hpp"
#include "ClientConnection.hpp"
#include "HasTcpConnection.hpp"
#include "Util/FileUtil.hpp"

#include <atomic>
#include <thread>
#include <iostream>

#include <msgpack.hpp>
#include <rpc/server.h>

BUFSTACK_BEGIN_NAMESPACE

class MsgpackRpclibTests
    : public ::testing::Test, 
    public Loggable
{
public:
    MsgpackRpclibTests()
        : Loggable("MsgpackRpclibTests")
    {}

    virtual ~MsgpackRpclibTests() {}


    class SetFlagTest
    {
    public:
        std::atomic_bool called;

        const std::string fName;
        const std::function<void(void)> f;

        SetFlagTest()
            : called(false),
            fName("foo"),
            f([this]() -> void {
                    this->called.store(true);
                })
        {}

        bool passes() const { return called.load(); }
    };
};

/**
 * use rpclib for both the server and client
 */
TEST_F(MsgpackRpclibTests, TestRpclibOnly)
{

}

TEST_F(MsgpackRpclibTests, TestCallVoidReturn)
{
    SetFlagTest flagTest;

    const auto port = TestConfig::rpclibTestPort;
    rpc::server server(HasTcpConnection::localhost, port);

    server.bind(flagTest.fName, flagTest.f);

    server.async_run();

    MockMsgpackClient client(
        ConnectionInfo::tcpConnection(HasTcpConnection::localhost,
            port));

    client.callVoidReturn(flagTest.fName);

    ASSERT_TRUE(flagTest.passes());

    server.stop();
    server.close_sessions();
} 


TEST_F(MsgpackRpclibTests, TestCallReturnString)
{
    try {
    const auto port = TestConfig::rpclibTestPort;
    rpc::server server(HasTcpConnection::localhost, port);

    const std::string msg = "hello, world!";
    const std::string fName = "foo";
    const auto f = [msg]() -> std::string {
        return msg;
    };

    server.bind(fName, f);

    server.async_run();

    MockMsgpackClient client(
        ConnectionInfo::tcpConnection(HasTcpConnection::localhost,
            port));

    std::string res = client.call<std::string>(fName);
    ASSERT_EQ(res, msg);

    server.stop();
    server.close_sessions();
    } catch(std::exception& e)
    {
        std::cerr << "Caught exception: " << e.what() << std::endl;
    }
} 

BUFSTACK_END_NAMESPACE
