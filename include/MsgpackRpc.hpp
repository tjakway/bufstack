#pragma once

#include <functional>
#include <sstream>

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
    public:
        enum Type
        {
            Request = 0,
            Response = 1,
            Notification = 2
        };

        NEW_EXCEPTION_TYPE_WITH_BASE(MessageFormatException, 
                MsgpackRpcException);

        const Type type;

        Type getType() const { return type; }

        std::string printMessage();
        static std::string printType(Type);

    protected:
        static Type intToType(int);

        Message(int _type)
            : type(intToType(_type))
        {}

        Message(Type _type)
            : type(_type)
        {}

        //minimum number of msgpack objects in a message
        //TODO: make constexpr
        static int getMinimumMessageLength();
    };


    class RequestMessage : public Message
    {
    public:
        const uint32_t msgId;
        const std::string method;
        const std::vector<std::reference_wrapper<msgpack::object>> params;

        RequestMessage(
            const std::string _method,
            const std::vector<
                std::reference_wrapper<msgpack::object>> _params)
            : Message(Type::Request),
            method(_method), params(_params)
        {}

        static const int messageSize;
    };

    class ResponseMessage : public Message
    {
    public:
        const uint32_t msgId;
        /**
            * nullopt if no error
            */
        const optional<std::reference_wrapper<msgpack::object>> error;
        const std::reference_wrapper<msgpack::object> result;

        ResponseMessage(
                uint32_t _msgId,
                optional<std::reference_wrapper<msgpack::object>> _error,
                std::reference_wrapper<msgpack::object> result)
            : Message(Type::Response),
            msgId(_msgId), error(_error), result(_result)
        {}

        bool error() const
        {
            return error.has_value();
        }

        std::string errorToString() const
        {
            std::ostringstream ss;
            if(error.has_value())
            {
                ss << error.value();
            }
            else
            {
                ss << "[no error]";
            }

            return ss.str();
        }

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

    class NotificationMessage : public Message
    {
    public:
        const std::string method;
        const std::vector<std::reference_wrapper<msgpack::object>> params;

        NotificationMessage(
            const std::string _method,
            const std::vector<
                std::reference_wrapper<msgpack::object>> _params)
            : Message(Type::Notification),
            method(_method), params(_params)
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
