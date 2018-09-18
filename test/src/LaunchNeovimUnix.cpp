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

std::pair<int,int> NvimConnectionTest::launchNeovim(
    const std::string& path)
{
    //create a pipe to communicate with the nvim child process
    int pipeFds[2];
    pipeFds[0] = -1;
    pipeFds[1] = -1;

    if(pipe(pipeFds) != 0)
    {
        auto _errno = errno;
        throw NvimLaunchException(
            STRCATS("Error calling pipe(2): " <<
                strerror(_errno)));
    }


    pid_t pid = fork();
    //child
    if(pid == 0)
    {
        //set the read end of the pipe to stdin and
        //the write end to stdout
        if(dup2(STDIN_FILENO, pipeFds[0]) == -1)
        {
            throw NvimLaunchException(
                STRCATS("Error setting the read end of the pipe (" <<
                    "file descriptor " << pipeFds[0] << ") to stdin"));
        }
        
        if(dup2(STDOUT_FILENO, pipeFds[1]) == -1)
        {
            throw NvimLaunchException(
                STRCATS("Error setting the write end of the pipe (" <<
                    "file descriptor " << pipeFds[1] << ") to stdout"));
        }

        int execRet = execl(path.c_str(), 
                //don't forget to pass the executable path as argv[0]
                path.c_str(), 
                "--embed"
                //"--noplugin", 
                //no vimrc or shada
                //"-u", "NONE", "-i", "NONE",
                "-V20my_log",
                NULL);
        
        //only reached if there's an error in exec
        auto _errno = errno;
        throw NvimLaunchException(
            STRCATS("Error launching nvim at path " << 
                path << " (execl returned " << execRet << ")"));
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
        return std::make_pair(pipeFds[0], pipeFds[1]);
    }
}

BUFSTACK_END_NAMESPACE
