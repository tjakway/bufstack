/**
 * unix-specific code to launch nvim and connect to it
 */
#include "NvimConnectionTest.hpp"

#include "Util/Strcat.hpp"

#include <string>
#include <iostream>

#include <cstring> //strerror(int)
#include <cstdlib> //exit(int)

#include <errno.h>
#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE

void NvimConnectionTest::launchNeovim(
    const std::string& path,
    const std::string& address,
    uint16_t port)
{
    const std::string addrString = 
        address + std::string(":") + std::to_string(port);

    int pid = fork();
    //child
    if(pid == 0)
    {
        char *const envp[] = [connectionString.c_str(), NULL];
        const char* envVar = "NVIM_LISTEN_ADDRESS";
        int ret = setenv(envVar, addrString.c_str(), 1);
        if(ret < 0)
        {
            const auto _errno = errno;
            const auto errStr = strerror(_errno);
            const char* errMsg = STRCATS("Error calling setenv with " << envVar
                << errStr << std::endl).c_str();
            std::cerr << errMsg;
            //just in case, print system wide error message
            perror(errMsg);
            exit(1);
        }
        execv(path, envp);
    }
    //parent
    else
    {

    }

    //connect to that address
}

BUFSTACK_END_NAMESPACE
