#include "MsgpackReceiver.hpp"

#include <type_traits>
#include <vector>

#include "NamespaceDefines.hpp"
#include "MsgpackRpc.hpp"

#include "Util/MsgpackUtil.hpp"
#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

void MsgpackReceiver::handleRequestMessage(
        const std::vector<msgpack::object>& msgObj)
{
    uint32_t msgId;
    msgObj.at(1).convert(msgId);

    std::string method;
    msgObj.at(2).convert(method);

    std::vector<std::reference_wrapper<msgpack::object>> params =
       MsgpackUtil::wrapObjects(msgObj.at(3).as<std::vector<msgpack::object>>);

    onReceiveRequestMsg(MsgpackRpc::RequestMessage(msgId, method, params));
}

void MsgpackReceiver::handleResponseMessage(
        const std::vector<msgpack::object>& msgObj)
{
    uint32_t msgId;
    msgObj.at(1).convert(msgId);
    
    const bool errorOccurred = msgObj.at(2).is_nil();

    optional<std::string> errorField;
    optional<std::reference_wrapper<msgpack::object>> resultField;

    if(errorOccurred)
    {
        if(!msgObj.at(3).is_nil())
        {
            throw MsgpackReceiverException(
                    STRCATS("result object should be " <<
                    "nil in a message of type " <<
                    MsgpackRpc::Message::printType(type) << 
                    " but result == " << msgObj.at(3)));
        }

        errorField = make_optional(STRCAT(msgObj.at(2)));
        resultField = nullopt;
    }
    else
    {
        errorField = nullopt;
        auto resultObj = msgObj.at(3);
        if(!resultObj.is_nil())
        {
            resultField = make_optional(std::ref(resultObj));
        }
    }

    onReceiveResponseMsg(
            MsgpackRpc::ResponseMessage(msgId, errorField, resultField));
}

void MsgpackReceiver::handleNotificationMessage(
        const std::vector<msgpack::object>& msgObj)
{
    std::string method;
    msgObj.at(1).convert(method);

    
    std::vector<msgpack::object> paramObjects;
    msgObj.at(2).convert(paramObjects);

    std::vector<std::reference_wrapper<msgpack::object>> params;
    params.reserve(paramObjects.size());
    for(auto& i : paramObjects)
    {
        params.emplace_back(std::reference_wrapper<msgpack::object>(i));
    }

    onReceiveNotificationMsg(
            MsgpackRpc::NotificationMessage(method, params));
}

void MsgpackReceiver::onRecvMsg(const msgpack::object& o)
{
    std::vector<msgpack::object> msgObj;

    getLogger()->info("Received msgpack object: {}", toString(o));

    if(o.type != msgpack::type::object_type::ARRAY)
    {
        throw NotMessageException(STRCATS(
            "Expected msgpack object of type " <<
            "ARRAY but got type code " << o.type));
    }
    else
    {
        o.convert(msgObj);

        if(msgObj.size() < MsgpackRpc::Message::getMinimumMessageLength())
        {
            throw MsgpackReceiverException(STRCATS("Expected a vector of size() >= " <<
                        MsgpackRpc::Message::getMinimumMessageLength() <<
                        " but size() == " << msgObj.size()));
        }

        //extract message type
        std::underlying_type<MsgpackRpc::Message::Type>::type enumIntType;
        MsgpackRpc::Message::Type type = 
            static_cast<MsgpackRpc::Message::Type>(msgObj.at(0).convert(enumIntType));

        const auto checkMessageLength = [&msgObj, &type](int expectedSize){
            if(msgObj.size() != expectedSize)
            {
                throw MsgpackReceiverException(STRCATS("Expected a vector of size " <<
                        expectedSize << 
                        " for a message of type " << 
                        MsgpackRpc::Message::printType(type) << 
                        " but got size() == " << msgObj.size()));
            }
        };

        switch(type)
        {
            case MsgpackRpc::Message::Type::Request:
            {
                checkMessageLength(MsgpackRpc::RequestMessage::messageSize);
                handleRequestMessage(msgObj);
                break;
            }

            case MsgpackRpc::Message::Type::Response:
            {
                checkMessageLength(MsgpackRpc::ResponseMessage::messageSize);
                handleResponseMessage(msgObj);
                break;
            }


            case MsgpackRpc::Message::Type::Notification:
            {
                checkMessageLength(MsgpackRpc::NotificationMessage::messageSize);
                handleNotificationMessage(msgObj);
                break;
            }

            default:
            {
                throw MsgpackReceiverException(STRCATS("Unknown message type for "
                            << "object " << Util::printVector(msgObj)));
            }
        }

    }
}

BUFSTACK_END_NAMESPACE
