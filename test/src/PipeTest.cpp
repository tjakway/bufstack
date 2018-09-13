#include "PipeTest.hpp"

#include "NamespaceDefines.hpp"
#include "Util/Util.hpp"

#include <gtest/gtest.h>

#include <cerrno>
#include <memory>
#include <unistd.h>
#include <cstring>
#include <sstream>
#include <string>
#include <iostream>


namespace {
    std::string printBuf(const char* data, std::size_t len)
    {
        std::stringstream ss;
        for(std::size_t i = 0; i < len; i++)
        {
            ss << data[0];
        }

        return ss.str();
    }
}

BUFSTACK_BEGIN_NAMESPACE


void PipeTest::writeCheck(const char* data, std::size_t len)
{
    ASSERT_GT(len, 0);
    ASSERT_NE(data, nullptr);

    ASSERT_TRUE(Util::fd_is_valid(writeFd));
    ASSERT_TRUE(Util::fd_is_valid(readFd));

    //results are undefined if >SSIZE_MAX, see write(2) and read(2)
    ASSERT_LE(len, SSIZE_MAX);

    std::size_t amountLeft = len;
    std::size_t offset = 0;

    while(amountLeft > 0)
    {
        ASSERT_LT(offset, len);

        ssize_t amountWritten = write(writeFd, &data[offset], amountLeft);
        auto _errno = errno;
        
        //error if write returns <0
        ASSERT_GT(amountWritten, -1) << strerror(_errno);

        amountLeft -= amountWritten;
        offset += amountWritten;

        ASSERT_GE(amountLeft, 0);

        ASSERT_LE(offset, len);
    }

    assertRead(data, len);
}

void PipeTest::assertRead(const char* data, std::size_t len)
{
    std::unique_ptr<char[]> readBuf(new char[len]);
    ssize_t amountRead = read(readFd, readBuf.get(), len);
    auto _errno = errno;

    ASSERT_EQ(amountRead, len) << strerror(_errno);

    ASSERT_EQ(memcmp(data, readBuf.get(), len), 0) 
        << "passed buf: < " << printBuf(data, len) << " >;"
        << "  read buf: < " << printBuf(readBuf.get(), len) << " >";
}

BUFSTACK_END_NAMESPACE
