#pragma once

#include <atomic>
#include <memory>
#include <string>

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include "TestConfig.hpp"
#include "HasTcpConnection.hpp"
#include "MsgpackClient.hpp"

BUFSTACK_BEGIN_NAMESPACE

class NvimConnectionTest : virtual public Loggable
{
    std::unique_ptr<pid_t> nvimPid;

    static constexpr auto localhost = HasTcpConnection::localhost;

    //atomically initialize the client
    void connect(
        const std::string& _address,
        uint16_t _port);
    void launchNeovim(
        const std::string& path,
        const std::string& _address,
        uint16_t _port);

public:
    NEW_EXCEPTION_TYPE(NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(CannotFindNeovimException,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NvimNotInitialized,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NvimLaunchException,
            NvimConnectionTestException);


    NvimConnectionTest(
        const std::string& _address = TestConfig::nvimConnectionAddress,
        uint16_t _port = TestConfig::nvimConnectionPort);

    virtual ~NvimConnectionTest();

    std::shared_ptr<MsgpackClient> getClientInstance();
};

BUFSTACK_END_NAMESPACE
