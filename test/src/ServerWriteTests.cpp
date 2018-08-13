#include <gtest/gtest.h>

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

#include <string>
#include <sstream>
#include <msgpack.hpp>

#include <iostream>

#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "Loggable.hpp"

#include "PipeTest.hpp"
#include "MockServer.hpp"
#include "MsgpackTestObject.hpp"

BUFSTACK_BEGIN_NAMESPACE

class ServerWriteTests : public PipeTest
{
public:
    MsgpackTestObject<std::string> helloWorld {std::string("hello, world!")};
};


TEST_F(ServerWriteTests, TestWriteHelloWorld)
{
    MockServer server;
    std::stringstream buf;
    std::string original = "hello, world!";

    msgpack::packer<std::stringstream> p(buf);
    p.pack_str(original.size());
    p.pack_str_body(original.c_str(), original.size());

    const std::string bufStr = buf.str();
    MockServer::sendAll(writeFd, bufStr.c_str(), 
            bufStr.size(), *this);
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
        //vecH.front().get().convert(x);
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
