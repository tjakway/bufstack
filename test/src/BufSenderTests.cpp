#include <gtest/gtest.h>

#include "PipeTest.hpp"
#include "BufSender.hpp"

#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"

#include <memory>
#include <cstddef>

BUFSTACK_BEGIN_NAMESPACE

TEST_F(PipeTest, TestWriteCheck_Small)
{
    const char* foo = "foo";
    writeCheck(foo, sizeof(foo));
}

TEST_F(PipeTest, TestWriteCheck_Large)
{
    std::size_t len = 2 ^ 15;
    std::unique_ptr<char[]> buf(new char[len]);
    ASSERT_EQ(memset(buf.get(), (int)'a', len), buf.get());

    writeCheck(buf.get(), len);
}

class BufSenderTests
    : public PipeTest
{
    class MockBufSender : public BufSender
    {
    public:
        MockBufSender()
            : Loggable("MockBufSender")
        {}

        virtual ~MockBufSender() {}
    };

    std::unique_ptr<BufSender> bufSender = make_unique<MockBufSender>();

public:
    //use BufSender::send to write data to the file descriptor
    //and assert that we can read it back
    void checkSend(const char* data, std::size_t len)
    {
        bufSender->send(writeFd, data, len);
        assertRead(data, len);
    }
};

TEST_F(BufSenderTests, TestSendSmall)
{
    const char* foo = "foo";
    checkSend(foo, sizeof(foo));
}




BUFSTACK_END_NAMESPACE
