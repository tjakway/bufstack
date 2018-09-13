#include <gtest/gtest.h>

#include "PipeTest.hpp"

#include "NamespaceDefines.hpp"

#include <cstddef>

BUFSTACK_BEGIN_NAMESPACE

class BufSenderTests
    : public PipeTest
{

};

TEST_F(BufSenderTests, TestWriteCheck)
{
    const char* foo = "foo";
    writeCheck(foo, sizeof(foo));
}

TEST_F(BufSenderTests, TestLargeWrite)
{
    std::size_t len = 2 ^ 15;
    std::unique_ptr<char[]> buf(new char[len]);
    ASSERT_EQ(memset(buf.get(), (int)'a', len), buf.get());

    writeCheck(buf.get(), len);
}

BUFSTACK_END_NAMESPACE
