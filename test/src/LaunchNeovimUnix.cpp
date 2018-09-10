/**
 * unix-specific code to launch nvim and connect to it
 */
#include "NvimConnectionTest.hpp"

#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

#include <string>
#include <iostream>

#include <cstring> 
#include <cstdlib> 

#include <errno.h>
#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE


NvimConnectionTest::NvimArgs NvimConnectionTest::mkNvimArgs(const std::string& path)
{
    const int numOptions = sizeof(nvimOptions);

    //2 extra args: argv[0] and a NULL at the end of the array
    const int arraySize = numOptions + 2;

    NvimArgs args(new char*[arraySize], NvimArgsDeleter(arraySize));

    memcpy(args[0], path.c_str(), path.size() + 1); //+1 for the null terminator

    for(int i = 1; i < numOptions; i++)
    {
        memcpy(args[i], nvimOptions[i - 1], sizeof(nvimOptions[i - 1]));
    }
    args[arraySize - 1] = NULL;

    return args;
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

        //format the argument array
        char *const nvimArgs[] = { NULL };
        
        int execRet = execl(path.c_str(), 
                //don't forget to pass the executable path as argv[0]
                path.c_str(), "--headless", NULL);
        
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
