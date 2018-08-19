#pragma once

#include <functional>

#include <cstdint>
#include "nonstd/optional.hpp"

class MsgpackRpc
{
    class Message
    {
        enum Type
        {
            Request = 0;
            Response = 1;
            Notification = 2;
        };
        const Type type;

        const uint32_t msgId;
        const optional<std::string> error;

        const std::reference_wrapper<msgpack::object> result;

        Message(Type _type, 
                uint32_t _msgId, 
                optional<std::string> _error,
                std::reference_wrapper<msgpack::object> _result)
            : type(_type), msgId(_msgId), error(_error),
            result(_result)
        {}

        Message(const Message& other)
            : type(other.type),
            msgId(other.msgId),
            error(other.error),
            result(other.result)
        {}
    };
};
