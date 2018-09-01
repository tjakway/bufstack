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
    static std::atomic<std::unique_ptr<Client>> client;
    static const std::string localhost;

    //atomically initialize the client
    void connect();
    void launchNeovim(
        const std::string& path,
        const std::string& _address = localhost,
        uint16_t _port);

protected:
    NEW_EXCEPTION_TYPE(CannotFindNeovimException);
    Client getClientInstance();

public:
    NvimConnectionTest()
        : Loggable("NvimConnectionTest")
    {}
};

BUFSTACK_END_NAMESPACE
