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
};

TEST_F(MsgpackRpclibTests, TestCallVoidReturn)
{
    std::atomic_bool called(false);

    const auto port = TestConfig::rpclibTestPort;
    rpc::server server(HasTcpConnection::localhost, port);

    const std::string fName = "foo";
    const auto f = [&called]() -> void {
        called.store(true);
    };

    server.bind(fName, f);

    server.async_run();

    MockMsgpackClient client(
        ConnectionInfo::tcpConnection(HasTcpConnection::localhost,
            port));

    client.callVoidReturn(fName);

    ASSERT_TRUE(called);

    server.stop();
    server.close_sessions();
} 


TEST_F(MsgpackRpclibTests, TestCallReturnString)
{
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
} 

BUFSTACK_END_NAMESPACE
