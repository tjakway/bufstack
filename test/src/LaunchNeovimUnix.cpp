#include "NvimConnectionTest.hpp"

#include <unistd.h>

BUFSTACK_BEGIN_NAMESPACE

void NvimConnectionTest::launchNeovim(
    const std::string& _address,
    uint16_t _port)
{
    //TODO: platform specific
    int pid = fork();
    if(pid == 0)
    {
        
    }
}

BUFSTACK_END_NAMESPACE
