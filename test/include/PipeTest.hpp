#pragma once

#include <gtest/gtest.h>


#include "Util/Util.hpp"
#include "Util/CloseLogError.hpp"
#include "Loggable.hpp"

#include <unistd.h>
#include <fcntl.h>
#include <cassert>

class PipeTest 
    : public ::testing::Test, 
    public bufstack::Loggable
{
public:
    int readFd, writeFd;
    
    PipeTest()
        : Loggable("PipeTest")
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

    virtual ~PipeTest()
    {
        SAFE_CLOSE_LOG_ERROR(readFd);
        SAFE_CLOSE_LOG_ERROR(writeFd);
    }
};
