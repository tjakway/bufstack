#include <gtest/gtest.h>

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

#include <memory>
#include <string>
#include <sstream>
#include <msgpack.hpp>
#include <atomic>
#include <thread>

#include <iostream>

#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"
#include "Loggable.hpp"

#include "MsgpackReaderUnpacker.hpp"
#include "MockMsgpackReaderUnpacker.hpp"
#include "PipeTest.hpp"
#include "MsgpackTestObject.hpp"
#include "MsgpackFdReader.hpp"
#include "SyncBufSender.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackFdReaderTests
    : public PipeTest
{
public:
    MsgpackFdReaderTests()
        : loggerInstance("MsgpackFdReaderTests")
    {}
    virtual ~MsgpackFdReaderTests() {}

    using Callback = MsgpackReaderUnpacker::Callback;
    using ObjectList = MsgpackReaderUnpacker::ObjectList;

    MsgpackTestObject<std::string> helloWorld {std::string("hello, world!")};

    Loggable loggerInstance;

    virtual std::shared_ptr<spdlog::logger> getLogger() const noexcept
    {
        return loggerInstance.getLogger();
    }
};


TEST_F(MsgpackFdReaderTests, TestWriteHelloWorld)
{
    SyncBufSender sender;
    std::string original = "hello, world!";

    msgpack::sbuffer buf;
    msgpack::packer<msgpack::sbuffer> p(buf);
    p.pack_str(original.size());
    p.pack_str_body(original.c_str(), original.size());


    std::atomic_bool callbackDone{false};

    bool foundTestObject = false;
    int vecSize;
    const msgpack::object_handle& expectedHandle = this->helloWorld.obj;
    const auto callback = [this, &expectedHandle, &foundTestObject, &vecSize,
        &callbackDone](
            const ObjectList& vecH)
    {
        vecSize = vecH.size();
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

        callbackDone.store(true);
    };

    MsgpackFdReader reader(readFd, callback);

    sender.send(writeFd, buf.data(), 
            buf.size());

    reader.asyncStartListening();
    close(writeFd);

    while(!callbackDone.load())
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    ASSERT_EQ(vecSize, 1);
    ASSERT_TRUE(foundTestObject);
    ASSERT_TRUE(callbackDone.load());

    close(readFd);
}

BUFSTACK_END_NAMESPACE
