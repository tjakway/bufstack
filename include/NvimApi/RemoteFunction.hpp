#pragma once

#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/NvimFunction.hpp"

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

template <typename T, typename... Args>
class AbstractRemoteApiFunction
{
protected:
    NEW_EXCEPTION_TYPE(RemoteApiFunctionError);
    NEW_EXCEPTION_TYPE_WITH_BASE(RemoteFunctionUninitiliazedError, RemoteApiFunctionError);
    NEW_EXCEPTION_TYPE_WITH_BASE(BadNameError, RemoteApiFunctionError);
    NEW_EXCEPTION_TYPE_WITH_BASE(NotInApiInfoError, RemoteApiFunctionError);

    bool initialized;

    std::string name;
    optional<NvimFunction> functionSpecification;
    std::shared_ptr<rpc::client> rpcClient;

    void checkName() const
    {
        //make sure we have a function name
        if(initialized && Util::StringTrim::trim_copy(name).empty())
        {
            throw BadNameError("RemoteApiFunction constructor "
                    "called with empty name");
        }
    }

    void check() const
    {
        if(!initialized)
        {
            throw RemoteFunctionUninitiliazedError(
                    "Uninitialized RemoteApiFunction called");
        }

        checkName();
    }

    optional<T> convert(const msgpack::object& obj)
    {
        T t;
        obj.convert(t);
        return make_optional(t);
    }

    AbstractRemoteApiFunction(bool _initialized,
            const std::string& _name,
            optional<NvimFunction> spec,
            std::shared_ptr<rpc::client> _client)
        : initialized(_initialized),
        name(_name), 
        functionSpecification(spec),
        rpcClient(_client)
    {
        checkName();
    }

    /**
     * throw a NotInApiInfoError if the api info doesn't contain the 
     * passed function specification
     */
    void checkApiInfo(const ApiInfo&);

public:
    //uninitialized ctor
    AbstractRemoteApiFunction()
        : AbstractRemoteApiFunction(false, "", nullopt, nullptr)
    {}

    AbstractRemoteApiFunction(const NvimFunction& spec, 
            std::shared_ptr<rpc::client> rpcClient,
            const ApiInfo& info)
        : AbstractRemoteApiFunction(true, spec.name, make_optional(spec), rpcClient)
    {
        checkApiInfo(info);
    }


    //ctor that skips the api info check
    AbstractRemoteApiFunction(const std::string& _name,
            std::shared_ptr<rpc::client> client)
        : AbstractRemoteApiFunction(true, _name, nullopt, client)
    {}
};

template <typename T, typename... Args>
class ResultRemoteApiFunction : public 
{
    using AbstractRemoteApiFunction::AbstractRemoteApiFunction;
    optional<T> call(const Args&... args) const
    {
        check();

        msgpack::object_handle h = rpcClient->call(name, &args...);
        return convert(h.get());
    }

    std::future<optional<T>> async_call(const Args&... args) const
    {
        check();

        const auto conv = [this](msgpack::object_handle h){
            return this->convert(h.get());
        };

        return runAfter(rpcClient->async_call(name, &args...), conv);
    }

    T operator()(const Args&... args) const
    {
        return call(args...);
    }
};


template <typename... Args>
class RemoteApiFunction<void, Args...>
{
    optional<void> convert(const msgpack::object& obj)
    {
        return nullopt;
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
        std::shared_ptr<rpc::client> client,
        const ApiInfo& info)
        : bufIsValid(bufIsValidSpec, client, info),
        subscribe(subscribeSpec, client, info)
    {}
};

BUFSTACK_END_NAMESPACE
