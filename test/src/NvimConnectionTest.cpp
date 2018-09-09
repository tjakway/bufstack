#include "NvimConnectionTest.hpp"

#include <msgpack.hpp>
#include <mutex>
#include <memory>

//process signaling
#include <sys/types.h>
#include <signal.h>
#include <cerrno>

#include "NamespaceDefines.hpp"
#include "FindNeovim.hpp"
#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "ConnectionInfo.hpp"
#include "MsgpackClient.hpp"

namespace {
    static std::mutex clientPtrMutex;
    static std::shared_ptr<bufstack::MsgpackClient> clientPtr = nullptr;
}

BUFSTACK_BEGIN_NAMESPACE

std::shared_ptr<MsgpackClient> NvimConnectionTest::getClientInstance()
{
    std::lock_guard<std::mutex> {clientPtrMutex};
    if(!clientPtr)
    {
        throw NvimNotInitialized("Nvim instance not initialized "
                "(did you remember to connect()?)");
    }
    else
    {
        return clientPtr;
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
        clientPtr = std::make_shared(
                ConnectionInfo::tcpConnection(address, port));
    }
}

NvimConnectionTest::NvimConnectionTest(
    const std::string& address,
    uint16_t port)
    : Loggable("NvimConnectionTest"),
    nvimPid(nullptr)
{
    connect(address, port);
}

NvimConnectionTest::~NvimConnectionTest()
{
    if(nvimPid != nullptr && (*nvimPid) > 0)
    {
        //signal the child process
        if(!kill(*nvimPid, SIGTERM))
        {
            auto _errno = errno;
            getLogger()->critical(
                "Failed to kill child nvim instance,"
                " error message: {}", strerror(_errno));
        }
    }
}

BUFSTACK_END_NAMESPACE
