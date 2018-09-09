#include <gtest/gtest.h>

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

#include <memory>
#include <string>
#include <sstream>
#include <msgpack.hpp>

#include <iostream>

#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"
#include "Loggable.hpp"

#include "MsgpackReaderUnpacker.hpp"
#include "MockMsgpackReaderUnpacker.hpp"
#include "PipeTest.hpp"
#include "MsgpackTestObject.hpp"
#include "SyncBufSender.hpp"

BUFSTACK_BEGIN_NAMESPACE

class PipeWriteTests 
    : public PipeTest
{
public:
    PipeWriteTests()
        : loggerInstance("PipeWriteTests")
    {}
    virtual ~PipeWriteTests() {}

    using Callback = MsgpackReaderUnpacker::Callback;
    using ObjectList = MsgpackReaderUnpacker::ObjectList;

    MsgpackTestObject<std::string> helloWorld {std::string("hello, world!")};


    //google test doesn't allow test fixtures to virtually inherit so we use
    //this hack to get around it
    Loggable loggerInstance;

    virtual std::shared_ptr<spdlog::logger> getLogger() const noexcept override
    {
        return loggerInstance.getLogger();
    }
};


TEST_F(PipeWriteTests, TestWriteHelloWorld)
{
    MockMsgpackReaderUnpacker reader;
    SyncBufSender sender;
    std::string original = "hello, world!";

    msgpack::sbuffer buf;
    msgpack::packer<msgpack::sbuffer> p(buf);
    p.pack_str(original.size());
    p.pack_str_body(original.c_str(), original.size());

    sender.send(writeFd, buf.data(), 
            buf.size());
    close(writeFd);

    bool foundTestObject = false;
    int vecSize;
    const msgpack::object_handle& expectedHandle = this->helloWorld.obj;
    const auto callback = [this, &expectedHandle, &foundTestObject, &vecSize](
            const ObjectList& vecH)
    {
        vecSize = vecH.size();
        this->getLogger()->set_level(spdlog::level::debug);
        this->getLogger()->debug("vecH: {}", Util::printVector(vecH));
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

    reader.readFd(readFd, callback);

    ASSERT_EQ(vecSize, 1);
    ASSERT_TRUE(foundTestObject);

    close(readFd);
}

BUFSTACK_END_NAMESPACE
