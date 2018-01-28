#include <gtest/gtest.h>

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

#include <string>
#include <sstream>
#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "Loggable.hpp"

#include "MockServer.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ServerWriteTests : public ::testing::Test, public Loggable
{
public:
    int readFd, writeFd;
    
    

    ServerWriteTests()
    {
        //test reading and writing across a pipe
        int pipeFds[2];
        
        auto ret = pipe(pipeFds);
        assert(ret == 0);
        readFd = pipeFds[0];
        writeFd = pipeFds[1];

        //make the read end non blocking
        fcntl(readFd, F_SETFL, O_NONBLOCK);
    }
};


TEST_F(ServerWriteTests, TestWriteBasicStringSynchronous)
{
    std::string toWrite {"hello, world"};

    MockServer server;
    MockServer::sendAll(writeFd, toWrite.c_str(), toWrite.size(), *this);
    close(writeFd);
    const auto callback = [&server](const std::vector<msgpack::object_handle>& )
    {

    };
    std::vector<char> readData = server.readFd(readFd);
    ASSERT_EQ(toWrite, std::string(readData.cbegin(), readData.cend()));

    close(readFd);
}

TEST_F(ServerWriteTests, TestWriteNothing)
{

}

BUFSTACK_END_NAMESPACE
