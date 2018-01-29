#include <gtest/gtest.h>

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

#include <string>
#include <sstream>
#include <msgpack.hpp>

#include <iostream>

#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "Loggable.hpp"

#include "MockServer.hpp"
#include "MsgpackTestObject.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ServerWriteTests : public ::testing::Test, public Loggable
{
public:
    int readFd, writeFd;

    MsgpackTestObject<std::string> helloWorld {std::string("hello, world!")}; 
    
    ServerWriteTests()
    {
        //test reading and writing across a pipe
        int pipeFds[2];
        
        auto ret = pipe(pipeFds);
        assert(ret == 0);
        readFd = pipeFds[0];
        writeFd = pipeFds[1];

        //make the read end non blocking
        //fcntl(readFd, F_SETFL, O_NONBLOCK);
    }
};


TEST_F(ServerWriteTests, TestWriteHelloWorld)
{
    MockServer server;
    MockServer::sendAll(writeFd, helloWorld.original.c_str(), 
            helloWorld.original.size(), *this);
    close(writeFd);

    bool foundTestObject = false;
    int vecSize;
    const msgpack::object_handle& expectedHandle = this->helloWorld.obj;
    const auto callback = [&expectedHandle, &server, &foundTestObject, &vecSize](
            const std::vector<msgpack::object_handle>& vecH)
    {
        vecSize = vecH.size();
        //can't compare object_handles for equality, need to compare
        //the references to the underlying objects
        std::string a, b;
        vecH.front().get().convert(a);
        expectedHandle.get().convert(b);
        if(a == b)
        {
            foundTestObject = true;
        }
        else
        {
            std::cout << "equality check failed.  printing objects:" << std::endl;
            for(const auto& o: vecH)
            {
                std::cout << "\t" << o.get() << std::endl;
            }
        }
    };

    server.readFd(readFd, callback);

    ASSERT_EQ(vecSize, 1);
    ASSERT_TRUE(foundTestObject);

    close(readFd);
}

TEST_F(ServerWriteTests, TestWriteNothing)
{

}

BUFSTACK_END_NAMESPACE
