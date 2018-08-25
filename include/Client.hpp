#pragma once

#include "Util/Util.hpp"
#include "Util/NewExceptionType.hpp"
#include "NamespaceDefines.hpp"
#include "ApiParser.hpp"

#include "nonstd/optional.hpp"
    
#include <cstdint>

#include <memory>

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class Client;

template <typename T, typename... Args>
class RemoteApiFunction
{
    optional<NvimFunction> functionSpecification;
    std::shared_ptr<Client> client;

protected:
    RemoteApiFunction(optional<NvimFunction> spec,
            std::shared_ptr<Client> _client)
        : functionSpecification(spec),
        client(_client)
    {}

    NEW_EXCEPTION_TYPE(RemoteApiFunctionError);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotInApiInfoError, RemoteApiFunctionError);

    /**
     * throw a NotInApiInfoError if the api info doesn't contain the 
     * passed function specification
     */
    void checkApiInfo(const ApiInfo&);

public:
    RemoteApiFunction(const NvimFunction& spec, 
            std::shared_ptr<Client> client
            const ApiInfo& info)
        : RemoteApiFunction(make_optional(spec), client)
    {
        checkApiInfo(info);
    }


    //ctor that skips the api info check
    RemoteApiFunction(std::shared_ptr<Client> client)
        : RemoteApiFunction(nullopt, client)
    {}

    //TODO: implement
    T call(const Args&... args);
    T async_call(const Args&... args);
};

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
