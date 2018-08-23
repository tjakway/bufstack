#pragma once

#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"
    
#include <cstdint>

#include <memory>

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

BUFSTACK_BEGIN_NAMESPACE

class Client
{
    std::unique_ptr<rpc::client> client;

    std::string address;
    uint16_t port;

protected:
    void onConnect();
    void checkFunctions(const std::unordered_set<NvimFunction>&);

public:
    Client(const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port),
            client(make_unique<rpc::client>(address, port))
    {}


};

BUFSTACK_END_NAMESPACE
