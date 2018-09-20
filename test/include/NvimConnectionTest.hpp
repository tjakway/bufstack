#pragma once

#include <atomic>
#include <memory>
#include <string>
#include <mutex>

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"
#include "FindNeovim.hpp"

#include "TestConfig.hpp"
#include "MsgpackClient.hpp"
#include "ClientConnection.hpp"

BUFSTACK_BEGIN_NAMESPACE

class NvimConnectionTest
{
    std::unique_ptr<pid_t> nvimPid;

    std::mutex connectionMutex;
    std::shared_ptr<ClientEmbeddedConnection> nvimConnection;


    static constexpr auto localhost = HasTcpConnection::localhost;

    FindNeovim findNeovim;

    std::shared_ptr<MsgpackClient> tryCreateClient(
        const std::string& _address,
        uint16_t _port);

    //atomically initialize the client
    void connect();
    void launchNeovim(
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

    std::unique_ptr<NvimClient> getClientInstance();
};

BUFSTACK_END_NAMESPACE
