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

void NvimConnectionTest::launchNeovim(
    const std::string& path)
{
    //we need 2 pipes:
    //the first: parent out -> child stdin
    //the second: child stdout -> parent read
    const std::size_t numPipeFds = 4;
    int pipeFds[numPipeFds];
    memset(&pipeFds, -1, sizeof(int) * numPipeFds);
    int* parentReadPipe = &pipeFds[0];
    int* childReadPipe = &pipeFds[2];

    if(pipe(parentReadPipe) != 0 ||
            pipe(childReadPipe) != 0)
    {
        auto _errno = errno;
        throw NvimLaunchException(
            STRCATS("Error calling pipe(2): " <<
                strerror(_errno)));
    }

    int childWrite = parentReadPipe[1],
        childRead = childReadPipe[0],
        parentRead = parentReadPipe[0],
        parentWrite = childReadPipe[1];

    //check file descriptors
    for(int i = 0; i < numPipeFds; i++)
    {
        if(!Util::fd_is_valid(pipeFds[i]))
        {
            throw NvimLaunchException(STRCATS(
                "Sanity check Util::fd_is_valid failed" <<
                " for fd " << pipeFds[i] << 
                "(pipeFds[" << i << "])"));
        }
    }


    //fork
    pid_t pid = fork();
    //child
    if(pid == 0)
    {
        //set the read end of the pipe to stdin and
        //the write end to stdout
        if(dup2(childRead, STDIN_FILENO) == -1)
        {
            throw NvimLaunchException(
                STRCATS("Error setting the read end of the pipe (" <<
                    "file descriptor " << childRead << ") to stdin"));
        }
        
        if(dup2(childWrite, STDOUT_FILENO) == -1)
        {
            throw NvimLaunchException(
                STRCATS("Error setting the write end of the pipe (" <<
                    "file descriptor " << childWrite << ") to stdout"));
        }

        int execRet = execl(path.c_str(), 
                //don't forget to pass the executable path as argv[0]
                path.c_str(), 
                "--embed",
                "--noplugin", 
                //no vimrc or shada
                "-u", "NONE", "-i", "NONE",
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
        std::lock_guard<std::mutex>{connectionMutex};
        nvimPid = make_unique<pid_t>(pid);
        nvimConnection = 
            std::make_shared<ClientEmbeddedConnection>(
                    FdWrapper(parentRead), FdWrapper(parentWrite));
    }
}

BUFSTACK_END_NAMESPACE
