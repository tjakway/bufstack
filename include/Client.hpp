#pragma once

#include "Util/Util.hpp"
#include "Util/NewExceptionType.hpp"
#include "NamespaceDefines.hpp"

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/Client_decl.hpp"

#include "NvimApi/RemoteFunction.hpp"

#include "nonstd/optional.hpp"
    
#include <cstdint>

#include <memory>

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE


class Client 
{
    std::shared_ptr<rpc::client> client;
    std::unique_ptr<RemoteFunctionInstances> remoteFunctions;

    std::string address;
    uint16_t port;

    static const std::string subscribedEvents;

protected:
    //TODO: set an atomic flag to make sure we haven't run anything twice
    void onConnect();

    void initializeRemoteFunctions(const ApiInfo&);
    void subscribeEvents();
    void checkFunctions(const std::unordered_set<NvimFunction>&);

public:
    Client(const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port),
            client(std::make_shared<rpc::client>(address, port))
    {}



};

BUFSTACK_END_NAMESPACE
