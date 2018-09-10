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

    static constexpr auto localhost = HasTcpConnection::localhost;
    static constexpr char nvimOptions[][] = 
        { "--headless", "--noplugin", "-u", "NONE" };

    FindNeovim findNeovim;
    std::unique_ptr<Loggable> logger;

    class NvimArgsDeleter
    {
        int arraySize;
    public:
        NvimArgsDeleter(int _arraySize)
            : arraySize(_arraySize)
        {}

        void operator()(char*[]* ptr)
        {
            //arraySize - 1 because the last entry is NULL
            for(int i = 0; i < (arraySize - 1); i++)
            {
                delete[] (*ptr)[i];
            }
            delete[] *ptr;

            delete ptr;
        }
    };
    using NvimArgs = std::unique_ptr<char*[], NvimArgsDeleter>;
    NvimArgs mkNvimArgs(const std::string& path);

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

    void killNvimInstance();

    std::shared_ptr<MsgpackClient> getClientInstance();
};

BUFSTACK_END_NAMESPACE
