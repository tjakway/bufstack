#include "MsgpackRpc.hpp"

#include "Util/Strcat.hpp"
#include "Util/PrintOptional.hpp"
#include "Util/Util.hpp"

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
