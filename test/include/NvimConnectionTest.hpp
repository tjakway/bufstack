#pragma once

#include <atomic>
#include <memory>
#include <string>

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "FindNeovim.hpp"

#include "TestConfig.hpp"
#include "HasTcpConnection.hpp"
#include "MsgpackClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

class NvimConnectionTest
{
    std::unique_ptr<pid_t> nvimPid;
    std::pair<int, int> pipeFds;

    static constexpr auto localhost = HasTcpConnection::localhost;

    FindNeovim findNeovim;

    std::shared_ptr<MsgpackClient> tryCreateClient(
        const std::string& _address,
        uint16_t _port);

    //atomically initialize the client
    void connect();
    std::pair<FdWrapper,FdWrapper> launchNeovim(
        const std::string& path);

protected:
    std::unique_ptr<Loggable> logger;

public:
    NEW_EXCEPTION_TYPE(NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(CannotFindNeovimException,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NvimNotInitialized,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NvimLaunchException,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(CannotConnectException,
            NvimConnectionTestException);


    NvimConnectionTest();

    virtual ~NvimConnectionTest();

    std::shared_ptr<MsgpackClient> getClientInstance();
};

BUFSTACK_END_NAMESPACE
