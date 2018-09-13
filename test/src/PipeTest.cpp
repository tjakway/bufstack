#include "PipeTest.hpp"

#include "NamespaceDefines.hpp"

#include <gtest/gtest.h>

#include <unistd.h>


BUFSTACK_BEGIN_NAMESPACE


void PipeTest::writeCheck(const char* data, std::size_t len)
{
    std::size_t amountLeft = len;
    std::size_t offset = 0;

    while(amountLeft > 0)
    {
        ssize_t amountWritten = write(writeFd, &data[offset], amountLeft);
        
        //error if write returns <0
        ASSERT_GT(amountWritten, -1);

        amountLeft -= amountWritten;
        offset += amountWritten;

        ASSERT_GE(amountLeft, 0);
        ASSERT_LT(offset, len);
    }
}

BUFSTACK_END_NAMESPACE
