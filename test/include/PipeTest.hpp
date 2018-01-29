#pragma once

#include <gtest/gtest.h>


#include "Util/Util.hpp"
#include "Loggable.hpp"

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

class PipeTest : public ::testing::Test, public bufstack::Loggable
{
public:
    int readFd, writeFd;
    
    PipeTest()
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

    ~PipeTest()
    {
        if(Util::fd_is_valid(readFd))
        {
            close(readFd);
        }
        if(Util::fd_is_valid(writeFd))
        {
            close(writeFd);
        }
    }
};
