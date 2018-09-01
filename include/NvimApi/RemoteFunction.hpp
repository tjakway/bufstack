#pragma once

#include "Util/NewExceptionType.hpp"
#include "Util/Util.hpp"
#include "NamespaceDefines.hpp"

#include "NvimApi/ApiParser.hpp"
#include "NvimApi/NvimFunction.hpp"

#include <msgpack.hpp>
//NOTE: must include msgpack before rpc
#include <rpc/client.h>


#define REMOTE_API_FUNCTION_CALL_IMPL( \
        SYNC_RETURN_TYPE, ASYNC_RETURN_TYPE, CONVERT_FUNCTION)  \
    SYNC_RETURN_TYPE call(const Args&... args) const \
    { \
        check(); \
        \
        msgpack::object_handle h = rpcClient->call(name, &args...); \
        return CONVERT_FUNCTION(h.get()); \
    } \
    \
    ASYNC_RETURN_TYPE async_call(const Args&... args) const \
    { \
        check(); \
    \
        const auto conv = [this](msgpack::object_handle h){ \
            return CONVERT_FUNCTION(h.get()); \
        }; \
        \
        return runAfter(rpcClient->async_call(name, &args...), conv); \
    } \
    \
    SYNC_RETURN_TYPE operator()(const Args&... args) const \
    { \
        return call(args...); \
    }


using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

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
class HasReturnValueRemoteApiFunction : public AbstractRemoteApiFunction
{
    template <typename X>
    static X convert(const msgpack::object& obj)
    {
        X x;
        obj.convert(x);
        return x;
    }
public:
    using AbstractRemoteApiFunction::AbstractRemoteApiFunction;

    T call(const Args&... args) const 
    { 
        check(); 
        
        msgpack::object_handle h = rpcClient->call(name, &args...); 
        return convert(h.get()); 
    }
    
    std::future<T> async_call(const Args&... args) const 
    { 
        check(); 
    
        const auto conv = [this](msgpack::object_handle h){ 
            return HasReturnValueRemoteApiFunction::convert(h.get()); 
        }; 
        
        return runAfter(rpcClient->async_call(name, &args...), conv); 
    } 

    T operator()(const Args&... args) const 
    { 
        return call(args...); 
    }
};


template <typename... Args>
class NoReturnRemoteApiFunction : public AbstractRemoteApiFunction
{
public:
    using AbstractRemoteApiFunction::AbstractRemoteApiFunction;

    void call(const Args&... args) const 
    { 
        check(); 
        
        rpcClient->call(name, &args...); 
    }
    
    void async_call(const Args&... args) const 
    { 
        check(); 
    
        //don't need a conversion step here because we're discarding
        //the return value
        rpcClient->async_call(name, &args...); 
    } 

    void operator()(const Args&... args) const 
    { 
        call(args...); 
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
    const HasReturnValueRemoteApiFunction<bool, std::string> bufIsValid;
    const NoReturnRemoteApiFunction<std::string> subscribe;

    RemoteFunctionInstances(
        std::shared_ptr<rpc::client> client,
        const ApiInfo& info)
        : bufIsValid(bufIsValidSpec, client, info),
        subscribe(subscribeSpec, client, info)
    {}
};

BUFSTACK_END_NAMESPACE
