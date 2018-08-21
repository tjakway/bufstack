#pragma once

#include <functional>

#include <msgpack.hpp>

#include <cstdint>

#include "nonstd/optional.hpp"

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class MsgpackRpc
{
public:
    NEW_EXCEPTION_TYPE(MsgpackRpcException);

    class Message
    {
    private:
        NEW_EXCEPTION_TYPE_WITH_BASE(MessageFormatException, 
                MsgpackRpcException);
        void checkCtorArgs();
        void resultError();
        void methodError();
        void paramsError();

    public:
        //TODO: represent messages using a tagged union
        enum Type
        {
            Request = 0,
            Response = 1,
            Notification = 2
        };
        const Type type;

        Type getType() { return type; }

        //minimum number of msgpack objects in a message
        static const int minimumMessageLength;

    protected:
        const optional<uint32_t> msgId;
        const optional<std::string> error;

         
        const optional<std::string> method;

        const optional<std::reference_wrapper<msgpack::object>> result;

        const std::vector<std::reference_wrapper<msgpack::object>> params;


    protected:
        Message(Type _type, 
                optional<uint32_t> _msgId, 
                optional<std::string> _error,
                optional<std::string> _method,
                optional<std::reference_wrapper<msgpack::object>> _result,
                std::vector<std::reference_wrapper<msgpack::object>> _params)
            : type(_type), msgId(_msgId), error(_error),
            method(_method),
            result(_result), params(_params)
        {}

    public:
        Message(const Message& other)
            : Message(other.type,
            other.msgId,
            other.error,
            other.method,
            other.result,
            other.params)
        {}

        std::string printMessage();
        static std::string printType(Type);
    };

    class Response : public Message
    {
    public:
        Response(Type type,
                uint32_t msgId,
                optional<std::string> error,
                optional<std::reference_wrapper<msgpack::object>> result)
            : Message(type, 
                    make_optional(msgId),
                    error,
                    nullopt,
                    result,
                    std::vector<std::reference_wrapper<msgpack::object>>{})
        {}

        Response(const Response& other)
            : Message(other)
        {}

        std::string getMethod() 
        { 
            return method.value();
        }

        std::vector<std::reference_wrapper<msgpack::object>> getParams()
        {
            return params;
        }

        static const int messageSize;
    };

    class Notification : public Message
    {
    public:
        Notification(Type type,
                std::string method,
                std::vector<std::reference_wrapper<msgpack::object>> params)
            : Message(type, 
                    nullopt,
                    nullopt,
                    make_optional(method),
                    nullopt,
                    params)
        {}


        Notification(const Notification& other)
            : Message(other)
        {}

        std::string getMethod() 
        { 
            return method.value();
        }

        std::vector<std::reference_wrapper<msgpack::object>> getParams()
        {
            return params;
        }

        static const int messageSize;
    };
};


BUFSTACK_END_NAMESPACE
