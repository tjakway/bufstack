#pragma once

#include <atomic>
#include <memory>
#include <string>

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include "Client.hpp"

BUFSTACK_BEGIN_NAMESPACE

class NvimConnectionTest : virtual public Loggable
{
    static const std::string localhost;

    //atomically initialize the client
    void connect(
        const std::string& _address,
        uint16_t _port);
    void launchNeovim(
        const std::string& path,
        const std::string& _address,
        uint16_t _port);

protected:

    NEW_EXCEPTION_TYPE(NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(CannotFindNeovimException,
            NvimConnectionTestException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NvimNotInitialized,
            NvimConnectionTestException);
    Client& getClientInstance();

public:
    NvimConnectionTest()
        : Loggable("NvimConnectionTest")
    {}
};

BUFSTACK_END_NAMESPACE
