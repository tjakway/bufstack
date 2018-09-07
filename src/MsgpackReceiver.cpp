#include "MsgpackReceiver.hpp"

#include <type_traits>

#include "NamespaceDefines.hpp"
#include "MsgpackRpc.hpp"

#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

void MsgpackReceiver::handleRequestMessage(const msgpack::object&)
{

}
void MsgpackReceiver::handleResponseMessage(const msgpack::object&)
{

}
void MsgpackReceiver::handleNotificationMessage(const msgpack::object&)
{

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
            //TODO
            case MsgpackRpc::Message::Type::Request:
                throw MsgpackReceiverException("Request messages not implemented.");

            case MsgpackRpc::Message::Type::Response:
            {
                checkMessageLength(MsgpackRpc::ResponseMessage::messageSize);

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
                break;
            }


            case MsgpackRpc::Message::Type::Notification:
            {
                checkMessageLength(MsgpackRpc::NotificationMessage::messageSize);

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
                break;
            }
        }

    }
}

BUFSTACK_END_NAMESPACE
