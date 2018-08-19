#include "Server.hpp"

#include <type_traits>

#include "NamespaceDefines.hpp"
#include "MsgpackRpc.hpp"

#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

void MsgpackServer::onRecvMsg(const msgpack::object& o)
{
    std::vector<msgpack::object> msgObj;

    info() << "Received msgpack object: " << o;

    if(o.type != msgpack::type::object_type::ARRAY)
    {
        throw NotMessageError(STRCATS(
            "Expected msgpack object of type " <<
            "ARRAY but got type code " << o.type));
    }
    else
    {
        o.convert(msgObj);

        if(msgObj.size() < MsgpackRpc::Message::minimumMessageLength)
        {
            throw MsgpackServerError(STRCATS("Expected a vector of size() >= " <<
                        MsgpackRpc::Message::minimumMessageLength <<
                        " but size() == " << msgObj.size()));
        }

        //extract message type
        std::underlying_type<MsgpackRpc::Message::Type>::type enumIntType;
        MsgpackRpc::Message::Type type = 
            static_cast<MsgpackRpc::Message::Type>(msgObj.at(0).convert(enumIntType));

        const auto checkMessageLength = [&msgObj, &type](int expectedSize){
            if(msgObj.size() != expectedSize)
            {
                throw MsgpackServerError(STRCATS("Expected a vector of size " <<
                        expectedSize << 
                        " for a message of type " << 
                        MsgpackRpc::Message::printType(type) << 
                        " but got size() == " << msgObj.size()));
            }
        };

        switch(type)
        {
            case MsgpackRpc::Message::Type::Request:
                throw MsgpackServerError("Request messages not implemented.");

            case MsgpackRpc::Message::Type::Response:
            {
                checkMessageLength(MsgpackRpc::Response::messageSize);

                uint32_t msgId;
                msgObj.at(1).convert(msgId);
                
                const bool errorOccurred = msgObj.at(2).is_nil();

                optional<std::string> errorField;
                optional<std::reference_wrapper<msgpack::object>> resultField;

                if(errorOccurred)
                {
                    if(!msgObj.at(3).is_nil())
                    {
                        throw MsgpackServerError(
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
                    resultField = make_optional(std::ref(msgObj.at(3)));
                }

                onReceiveResponseMsg(
                        MsgpackRpc::Response(type, msgId, errorField, resultField));
                break;
            }


            case MsgpackRpc::Message::Type::Notification:
            {
                checkMessageLength(MsgpackRpc::Notification::messageSize);

                std::string method;
                msgObj.at(1).convert(method);

                
                std::vector<msgpack::object> paramObjects;
                msgObj.at(2).convert(paramObjects);

                std::vector<std::reference_wrapper<msgpack::object>> params;
                params.reserve(paramObjects.size());
                for(const auto i : paramObjects)
                {
                    params.emplace_back(std::ref(i));
                }

                onReceiveNotificationMsg(MsgpackRpc::Notification(type, method, params));
                break;
            }
        }

    }
}

BUFSTACK_END_NAMESPACE
