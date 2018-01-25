#include <gtest/gtest.h>

#include <unistd.h>

#include "Server.hpp"

class AsyncWriteServerTests
{
public:
    int readFd, writeFd;

    AsyncWriteServerTests()
    {
        //test reading and writing across a pipe
        int pipeFds[2];
        
        ASSERT_EQ(pipe(pipeFds, 0));
        readFd = pipeFds[0];
        writeFd = pipeFds[1];
    }
}

TEST_F(AsyncWriteServerTests, TestWriteNothing)
{

}
