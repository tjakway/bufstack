#include <gtest/gtest.h>

#include <msgpack.hpp>
#include <iostream>

#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "FindNeovim.hpp"
#include "MockServer.hpp"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

BUFSTACK_BEGIN_NAMESPACE

class ReadApiInfoTests : public ::testing::Test
{
public:
    const std::string apiInfoFilename {"api_info"};
};

TEST_F(ReadApiInfoTests, TestReadApiInfo)
{
    int readFd = open(apiInfoFilename.c_str(), O_RDONLY);
    ASSERT_GT(readFd, 0) << strerror(errno);

    bool receivedMessage;
    const auto callback = [&receivedMessage](const std::vector<msgpack::object_handle>& vecH)
    { 
        if(!vecH.empty())
        {
            receivedMessage = true;
        }
    };
    MockServer server;
    server.readFd(readFd, callback);
    ASSERT_TRUE(receivedMessage);
}

BUFSTACK_END_NAMESPACE
