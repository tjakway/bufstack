#include "NvimConnectionTest.hpp"

#include "NamespaceDefines.hpp"
#include "FindNeovim.hpp"

BUFSTACK_BEGIN_NAMESPACE

std::atomic<std::unique_ptr<Client>> NvimConnectionTest::client {nullptr};
const std::string NvimConnectionTest::localhost = "127.0.0.1";



void NvimConnectionTest::connect()
{
    auto nvimDest = FindNeovim::getFirstOnPath();
    if(nvimDest == nullptr)
    {
        throw CannotFindNeovimException("Could not find neovim on path");
    }
    else
    {
        const std::string nvimPath = *nvimDest;

    }
}

BUFSTACK_END_NAMESPACE
