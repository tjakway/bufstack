#pragma once

#include "Util/Util.hpp"
#include "Util/NewExceptionType.hpp"
#include "NamespaceDefines.hpp"
#include "NvimApi/ApiParser.hpp"

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
            std::shared_ptr<Client> client,
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

class RemoteFunctionInstances
{
protected:
    //TODO: implement specification checking
    //TODO: add a subclass of NvimFunction `NvimFunctionSpec` to be used for this
    //so we don't have to keep writing in irrelevant fields
    //TODO: should be able to automatically deduce parts of the spec from 
    //RemoteApiFunction template args
    const NvimFunction bufIsValidSpec = 
        NvimFunction(make_optional(true),
                make_optional(std::string("Boolean")),
                nullopt,
                nullopt,
                std::vector<std::string>{"Buffer"},
                "nvim_buf_is_valid");

    const NvimFunction subscribeSpec =
        NvimFunction(make_optional(true),
                nullopt, //TODO
                nullopt, //TODO
                nullopt,
                std::vector<std::string>{"Events"},
                "nvim_subscribe");

public:
    //TODO: wrap nvim types
    const RemoteApiFunction<bool, std::string> bufIsValid;
    const RemoteApiFunction<bool, std::string> subscribe;

    RemoteFunctionInstances(
        std::shared_ptr<Client> client,
        const ApiInfo& info)
        : bufIsValid(bufIsValidSpec, client, info),
        subscribe(subscribeSpec, client, info)
    {}
};

class Client : public std::enable_shared_from_this<Client>
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
