#include "NvimConnectionTest.hpp"

#include <msgpack.hpp>
#include <mutex>

#include "NamespaceDefines.hpp"
#include "FindNeovim.hpp"
#include "Client.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"

namespace {
    static std::mutex clientPtrMutex;
    static std::unique_ptr<bufstack::Client> clientPtr = nullptr;

}

BUFSTACK_BEGIN_NAMESPACE

//std::atomic<std::unique_ptr<Client>> NvimConnectionTest::client {nullptr};
const std::string NvimConnectionTest::localhost = "127.0.0.1";

Client& NvimConnectionTest::getClientInstance()
{
    std::lock_guard<std::mutex> {clientPtrMutex};
    if(!clientPtr)
    {
        throw NvimNotInitialized("Nvim instance not initialized "
                "(did you remember to connect()?)");
    }
    else
    {
        return *clientPtr;
    }
}


void NvimConnectionTest::connect(
        const std::string& address,
        uint16_t port)

{
    std::lock_guard<std::mutex> {clientPtrMutex};
    //only connect once
    if(clientPtr)
    {
        return;
    }

    auto nvimDest = FindNeovim::getFirstOnPath();
    if(nvimDest == nullptr)
    {
        throw CannotFindNeovimException("Could not find neovim on path");
    }
    else
    {
        const std::string nvimPath = *nvimDest;
        //launch neovim then connect the client to that address and port
        launchNeovim(nvimPath.c_str(), address, port);
        clientPtr = make_unique<Client>(address, port);
    }
}

BUFSTACK_END_NAMESPACE
