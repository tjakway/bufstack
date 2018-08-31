#pragma once

#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

BUFSTACK_BEGIN_NAMESPACE

template <typename T, typename... Args>
class RemoteApiFunction
{
    const std::string name;
    optional<NvimFunction> functionSpecification;
    std::shared_ptr<rpc::client> rpcClient;

protected:
    RemoteApiFunction(const std::string& _name,
            optional<NvimFunction> spec,
            std::shared_ptr<rpc::client> _client)
        : name(_name), 
        functionSpecification(spec),
        rpcClient(_client)
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
            std::shared_ptr<rpc::client> rpcClient,
            const ApiInfo& info)
        : RemoteApiFunction(spec.name, make_optional(spec), rpcClient)
    {
        checkApiInfo(info);
    }


    //ctor that skips the api info check
    RemoteApiFunction(const std::string& _name,
            std::shared_ptr<Client> client)
        : RemoteApiFunction(_name, nullopt, client)
    {}


    //TODO: implement
    T call(const Args&... args)
    {
        T t;
        msgpack::object_handle h = rpcClient->call(name, &args...);
        h.get().convert(t);
        return t;
    }

    std::future<T> async_call(const Args&... args)
    {
        const auto conv = [](msgpack::object_handle h){
            T t;
            h.get().convert(t);
            return t;
        };

        return runAfter(rpcClient->async_call(name, &args...), conv);
    }

    T operator()(const Args&... args)
    {
        return call(&args...);
    }
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
    const RemoteApiFunction<void, std::string> subscribe;

    RemoteFunctionInstances(
        std::shared_ptr<Client> client,
        const ApiInfo& info)
        : bufIsValid(bufIsValidSpec, client, info),
        subscribe(subscribeSpec, client, info)
    {}
};

BUFSTACK_END_NAMESPACE
