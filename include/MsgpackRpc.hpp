#pragma once

#include <functional>

#include <msgpack.hpp>

#include <cstdint>

#include "nonstd/optional.hpp"

#include "Util/NewExceptionType.hpp"

using namespace nonstd;

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

    public:
        enum Type
        {
            Request = 0,
            Response = 1,
            Notification = 2
        };
        const Type type;

        const uint32_t msgId;
        const optional<std::string> error;

         
        const optional<std::reference_wrapper<msgpack::object>> result;

        const std::vector<std::reference_wrapper<msgpack::object>> params;

        Message(Type _type, 
                uint32_t _msgId, 
                optional<std::string> _error,
                optional<std::reference_wrapper<msgpack::object>> _result,
                std::vector<std::reference_wrapper<msgpack::object>> _params)
            : type(_type), msgId(_msgId), error(_error),
            result(_result), params(_params)
        {}

        Message(const Message& other)
            : Message(other.type,
            other.msgId,
            other.error,
            other.result,
            other.params)
        {}
    };
};
