#include <gtest/gtest.h>

#include <msgpack.hpp>
#include <functional>
#include <ostream>

#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "FindNeovim.hpp"
#include "MockServer.hpp"
#include "Util/Util.hpp"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

BUFSTACK_BEGIN_NAMESPACE

class ReadApiInfoTests : public ::testing::Test
{
public:
    int readFd;
    static const std::string apiInfoFilename;

    //can't use assertions in CTOR
    ReadApiInfoTests()
        : readFd(-1)
    { }

    virtual void SetUp()
    {
        readFd = open(ReadApiInfoTests::apiInfoFilename.c_str(), O_RDONLY);
        ASSERT_GT(readFd, 0) << std::string(strerror(errno));
        return;
    }

    ~ReadApiInfoTests()
    {
        if(Util::fd_is_valid(readFd))
        {
            close(readFd);
        }
    }


    void readExpect(std::function<bool(const std::vector<msgpack::object_handle>&)> expect)
    {
        bool receivedMessage = false;
        bool expected = false;
        const auto callback = [&receivedMessage, &expected, expect](const std::vector<msgpack::object_handle>& vecH)
        { 
            if(!vecH.empty())
            {
                receivedMessage = true;
            }

            bool passed = expect(vecH);
            if(passed)
            {
                expected = true;
            }
        };
        MockServer server;
        server.readFd(readFd, callback);
        EXPECT_TRUE(receivedMessage);
        EXPECT_TRUE(expected);
    }
};
const std::string ReadApiInfoTests::apiInfoFilename {"resources/api_info"};


//make sure we can decode it without crashing
TEST_F(ReadApiInfoTests, TestCallbackInvoked)
{
    readExpect([](const std::vector<msgpack::object_handle>&){ return true; });
}



BUFSTACK_END_NAMESPACE
