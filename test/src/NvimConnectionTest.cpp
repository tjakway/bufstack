#include "NvimConnectionTest.hpp"

#include <msgpack.hpp>
#include <mutex>
#include <memory>
#include <iostream>
#include <chrono>
#include <thread>

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
#include "TestConfig.hpp"
#include "MockNvimClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

std::unique_ptr<NvimClient> NvimConnectionTest::getClientInstance()
{
    std::lock_guard<std::mutex> {connectionMutex};
    if(!nvimConnection)
    {
        throw NvimNotInitialized("Nvim instance not initialized "
                "(did you remember to connect()?)");
    }
    else
    {
        return make_unique<MockNvimClient>(nvimConnection);
    }
}


void NvimConnectionTest::connect()
{
    std::lock_guard<std::mutex> {connectionMutex};
    //only connect once
    if(!nvimConnection)
    {
        auto nvimDest = findNeovim.getFirstOnPath();
        if(nvimDest == nullptr)
        {
            throw CannotFindNeovimException("Could not find neovim on path");
        }
        else
        {
            const std::string nvimPath = *nvimDest;
            //launch neovim then connect the client to that address and port
            launchNeovim(nvimPath.c_str());

            if(nvimPid != nullptr)
            {
                logger->getLogger()->debug("Launched nvim instance with pid " + 
                        std::to_string(*nvimPid));
            }
        }
    }
    else
    {
        logger->getLogger()->debug("Already have an nvim connection, "
                "skipping call to connect");
    }

}

NvimConnectionTest::NvimConnectionTest()
    : logger(make_unique<Loggable>("NvimConnectionTest")), nvimPid(nullptr)
{
    //set log levels before connecting
    logger->getLogger()->set_level(spdlog::level::debug);
    findNeovim.getLogger()->set_level(spdlog::level::warn);

    this->connect();
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
