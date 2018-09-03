#include "MsgpackRpc.hpp"

#include "Util/Strcat.hpp"
#include "Util/PrintOptional.hpp"
#include "Util/Util.hpp"
#include "Util/PrintOptional.hpp"

#include <utility>
#include <map>
#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

std::string MsgpackRpc::Message::printType(MsgpackRpc::Message::Type t)
{
    switch(t)
    {
        case MsgpackRpc::Message::Type::Request:
            return std::string("Request");

        case MsgpackRpc::Message::Type::Response:
            return std::string("Response");

        case MsgpackRpc::Message::Type::Notification:
            return std::string("Notification");
    }

}

MsgpackRpc::Message::Type MsgpackRpc::Message::intToType(int i)
{
    switch(i)
    {
        case MsgpackRpc::Message::Type::Request:
            return MsgpackRpc::Message::Type::Request;

        case MsgpackRpc::Message::Type::Response:
            return MsgpackRpc::Message::Type::Response;

        case MsgpackRpc::Message::Type::Notification:
            return MsgpackRpc::Message::Type::Notification;

        default:
            throw MessageFormatException(STRCATS(
                    "Unknown message type < " << i << " >"));
    }
}

PrintableObject::Fields MsgpackRpc::RequestMessage::getFields() const noexcept
{
    return PrintableObject::Fields {
        std::make_pair("msgId", std::to_string(msgId)),
        std::make_pair("method", method),
        std::make_pair("params", Util::printVector(params))
    };
}
std::string MsgpackRpc::RequestMessage::getName() const noexcept
{
    return "RequestMessage";
}

PrintableObject::Fields MsgpackRpc::ResponseMessage::getFields() const noexcept
{
    return PrintableObject::Fields {
        std::make_pair("msgId", std::to_string(msgId)),
        std::make_pair("error", printOptional(error)),
        std::make_pair("result", STRCAT(result))
    };
}
std::string MsgpackRpc::ResponseMessage::getName() const noexcept
{
    return "ResponseMessage";
}

PrintableObject::Fields MsgpackRpc::NotificationMessage::getFields() const noexcept
{
    return PrintableObject::Fields {
        std::make_pair("method", method),
        std::make_pair("params", Util::printVector(params))
    };
}
std::string MsgpackRpc::NotificationMessage::getName() const noexcept
{
    return "NotificationMessage";
}

const int MsgpackRpc::RequestMessage::messageSize = 4;
const int MsgpackRpc::ResponseMessage::messageSize = 4;
const int MsgpackRpc::NotificationMessage::messageSize = 3;

int MsgpackRpc::Message::getMinimumMessageLength()
{
    return std::min(
        RequestMessage::messageSize,
        std::min(ResponseMessage::messageSize,
                 NotificationMessage::messageSize));
}


BUFSTACK_END_NAMESPACE
