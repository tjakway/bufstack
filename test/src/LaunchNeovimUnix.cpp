/**
 * unix-specific code to launch nvim and connect to it
 */
#include "NvimConnectionTest.hpp"

#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

#include <string>
#include <iostream>
#include <functional>

#include <cstring> //strerror(int)
#include <cstdlib> //exit(int)
#include <cstdio>

#include <errno.h>
#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE

namespace {

    void redirectFds(const std::string& stdoutDest, 
            const std::string& stderrDest,
            std::function<void(std::string)> onError)
    {
        const int flags = O_APPEND | O_WRONLY;

        //open the new file descriptors
        int newStdoutFd = open(stdoutDest.c_str(), flags);
        if(newStdoutFd < 0)
        {
            auto _errno = errno;
            onError(STRCATS("failed to open file descriptor at " <<
                        stdoutDest << ", error message: " <<
                        strerror(_errno)));
        }

        int newStderrFd = open(stderrDest.c_str(), flags);
        if(newStderrFd < 0)
        {
            auto _errno = errno;
            onError(STRCATS("failed to open file descriptor at " <<
                        stderrDest << ", error message: " <<
                        strerror(_errno)));
        }

        //swap the old ones
        if(dup2(STDOUT_FILENO, newStdoutFd) != newStdoutFd)
        {
            auto _errno = errno;
            onError(STRCATS("could not swap stdout file descriptor," <<
                        " error message: " << strerror(_errno)));
        }

        if(dup2(STDERR_FILENO, newStderrFd) != newStderrFd)
        {
            auto _errno = errno;
            onError(STRCATS("could not swap stderr file descriptor," <<
                        " error message: " << strerror(_errno)));
        }
    }
}

void NvimConnectionTest::launchNeovim(
    const std::string& path,
    const std::string& address,
    uint16_t port)
{
    const std::string addrString = 
        address + std::string(":") + std::to_string(port);

    pid_t pid = fork();
    //child
    if(pid == 0)
    {
        const char* envVar = "NVIM_LISTEN_ADDRESS";
        int ret = setenv(envVar, addrString.c_str(), 1);
        if(ret < 0)
        {
            //print an error message instead of throwing an exception
            //because we've already forked
            const auto _errno = errno;
            const auto errStr = strerror(_errno);
            const char* errMsg = STRCATS("Error calling setenv with " << envVar
                << errStr << std::endl).c_str();
            std::cerr << errMsg;
            //just in case, print system wide error message
            perror(errMsg);
            exit(1);
        }

        redirectFds("/dev/null", "/dev/null", 
                [](std::string msg) { throw NvimLaunchException(msg); });

        int execRet = execl(path.c_str(), 
                //don't forget to pass the executable path as argv[0]
                path.c_str(), "--headless", "--noplugin", "-u", "NONE", NULL);
        
        //only reached if there's an error in exec
        auto _errno = errno;
        throw NvimLaunchException(
            STRCATS("Error launching nvim at path " << 
                path << " with NVIM_LISTEN_ADDRESS=" << 
                addrString << " (execl returned " << execRet << ")"));
    }
    else if(pid < 0)
    {
        auto _errno = errno;
        throw NvimLaunchException(
            STRCATS("Fork returned <0, error message: " << 
                strerror(_errno)));
    }
    //parent
    else
    {
        nvimPid = make_unique<pid_t>(pid);
    }
}

BUFSTACK_END_NAMESPACE
