#pragma once

#include <functional>
#include <sstream>

#include <msgpack.hpp>

#include <cstdint>

#include "nonstd/optional.hpp"

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"
#include "Util/PrintableObject.hpp"

using namespace nonstd;

BUFSTACK_BEGIN_NAMESPACE

class MsgpackRpc
{
public:
    NEW_EXCEPTION_TYPE(MsgpackRpcException);

    class Message : public PrintableObject
    {
    public:
        using EnumType = uint8_t;
        enum Type : EnumType
        {
            Request = 0,
            Response = 1,
            Notification = 2
        };

        NEW_EXCEPTION_TYPE_WITH_BASE(MessageFormatException, 
                MsgpackRpcException);

        const Type type;

        static EnumType getTypeValue(Type t) { return static_cast<EnumType>(t); }
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

    public:
        //minimum number of msgpack objects in a message
        //TODO: make constexpr
        static int getMinimumMessageLength();
    };


    class RequestMessage : public Message
    {
    public:
        const uint32_t msgId;
        const std::string method;
        const std::vector<std::reference_wrapper<const msgpack::object>> params;

        RequestMessage(
            uint32_t _msgId,
            const std::string& _method,
            const std::vector<
                std::reference_wrapper<const msgpack::object>>& _params)
            : Message(Type::Request),
            msgId(_msgId),
            method(_method), params(_params)
        {}

        static const int messageSize;


        virtual Fields getFields() const noexcept override;
        virtual std::string getName() const noexcept override;
    };

    class ResponseMessage : public Message
    {
    public:
        const uint32_t msgId;
        /**
            * nullopt if no error
            */
        const optional<std::string> error;
        const optional<std::reference_wrapper<msgpack::object>> result;

        ResponseMessage(
                uint32_t _msgId,
                optional<std::string> _error,
                optional<std::reference_wrapper<msgpack::object>> _result)
            : Message(Type::Response),
            msgId(_msgId), error(_error), result(_result)
        {}

        bool isError() const
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
        
        static const int messageSize;


        virtual Fields getFields() const noexcept override;
        virtual std::string getName() const noexcept override;
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
            return method;
        }

        std::vector<std::reference_wrapper<msgpack::object>> getParams()
        {
            return params;
        }

        static const int messageSize;


        virtual Fields getFields() const noexcept override; 
        virtual std::string getName() const noexcept override;
    };

};


BUFSTACK_END_NAMESPACE
