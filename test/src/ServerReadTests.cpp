#include <gtest/gtest.h>

#include <msgpack.hpp>
#include <iostream>

#include "NamespaceDefines.hpp"
#include "Server.hpp"
#include "FindNeovim.hpp"
#include "MockServer.hpp"

#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE

TEST(ServerReadTests, TestReadApiInfo)
{
    int pipeFds[2];
    int readFd = pipeFds[0];
    int writeFd = pipeFds[1];

    auto ret = FindNeovim::getFirstOnPath();
    ASSERT_NE(ret, nullptr);
    std::string neovimLoc = *ret;

    pid_t pid = fork();
    ASSERT_GE(pid, 0);

    if(pid == 0)
    {
        //we're the child
        
        close(readFd);
        //replace stdout with the write end of the pipe
        dup2(STDOUT_FILENO, writeFd);

        execl(neovimLoc.c_str(), neovimLoc.c_str(), "--api-info", (char*)nullptr);
    }
    else
    {
        //we're the parent
        close(writeFd);
        //wait until the child exits
        waitpid(pid, nullptr, 0);

        //read from the pipe

        const auto callback = [](const std::vector<msgpack::object_handle>& vecH)
        {
            std::cout << "got the message" << std::endl;
        };
        MockServer server;
        server.readFd(readFd, callback);

        close(readFd);
    }
}

BUFSTACK_END_NAMESPACE
