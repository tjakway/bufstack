#include "NvimConnectionTest.hpp"

#include <msgpack.hpp>
#include <mutex>
#include <memory>
#include <iostream>

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
#include "MockMsgpackClient.hpp"

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

    auto nvimDest = findNeovim.getFirstOnPath();
    if(nvimDest == nullptr)
    {
        throw CannotFindNeovimException("Could not find neovim on path");
    }
    else
    {
        const std::string nvimPath = *nvimDest;
        //launch neovim then connect the client to that address and port
        launchNeovim(nvimPath.c_str(), address, port);

        if(nvimPid != nullptr)
        {
            getLoggableInstance()->getLogger()->debug("Launched nvim instance with pid " + 
                    std::to_string(*nvimPid));
        }

        clientPtr = std::make_shared<MockMsgpackClient>(
                ConnectionInfo::tcpConnection(address, port));
    }
}

NvimConnectionTest::NvimConnectionTest(
    const std::string& address,
    uint16_t port)
    : nvimPid(nullptr)
{
    connect(address, port);
    findNeovim.getLogger()->set_level(spdlog::level::warn);
}

NvimConnectionTest::~NvimConnectionTest()
{
    if(nvimPid != nullptr && (*nvimPid) > 0)
    {
        //signal the child process
        if(kill(*nvimPid, SIGTERM) != 0)
        {
            auto _errno = errno;
            //calls to pure virtual functions are not available in destructors
            //so we can't use getLoggableInstance() here
            std::cerr << "Failed to kill child nvim instance," <<
                " error message: " << strerror(_errno) << std::endl;
        }
    }
}

BUFSTACK_END_NAMESPACE
