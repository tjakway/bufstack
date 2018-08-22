#pragma once

#include "Util/Util.hpp"
    
#include <cstdint>

#include <memory>

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

class Connect
{
    std::unique_ptr<rpc::client> client;

    std::string address;
    uint16_t port;

    msgpack::object_handle getApiInfo();

public:
    Connect(const std::string& _address,
            uint16_t _port)
        : address(_address), port(_port),
            client(make_unique<rpc::client>(address, port))
    {}


};
