/**
 * unix-specific code to launch nvim and connect to it
 */
#include "NvimConnectionTest.hpp"

#include <string>

#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE

void NvimConnectionTest::launchNeovim(
    const std::string& path,
    const std::string& address,
    uint16_t port)
{
    const std::string connectionString = 
        std::string("NVIM_LISTEN_ADDRESS=") + 
        address + std::string(":") + std::to_string(port);

    int pid = fork();
    //child
    if(pid == 0)
    {
        char *const envp[] = [connectionString.c_str(), NULL];
        execvpe(path, nullptr, envp);
    }
    //parent
    else
    {

    }

    //connect to that address
}

BUFSTACK_END_NAMESPACE
