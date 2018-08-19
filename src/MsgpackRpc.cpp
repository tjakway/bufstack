#include "MsgpackRpc.hpp"

#include "Util/Strcat.hpp"
#include "Util/PrintOptional.hpp"
#include "Util/Util.hpp"

BUFSTACK_BEGIN_NAMESPACE

namespace {

    std::string printType(MsgpackRpc::Message::Type t)
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
}

void MsgpackRpc::Message::resultError()
{
    throw MessageFormatException(
        STRCATS("Did not expect result for message of type " <<
            printType(type) << 
            " but result == " << printOptional(result)));
}

void MsgpackRpc::Message::paramsError()
{
    throw MessageFormatException(
        STRCATS("Did not expect params for message of type " <<
            printType(type) << 
            " but params == " << Util::printVector(params)));
}

void MsgpackRpc::Message::methodError()
{
    throw MessageFormatException(
        STRCATS("Did not expect a method name for message of type " <<
            printType(type) << 
            " but method == " << printOptional(method)));
}

void MsgpackRpc::Message::checkCtorArgs()
{
    //see https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md for reference

    switch(type)
    {
        case Request:
            if(result.has_value())
            {
                resultError();
            }

            break;

        case Response:
            if(method.has_value())
            {
                methodError();
            }
            if(params.size() > 0)
            {
                paramsError();
            }

            //if an error occurred the result should be nil
            if(error.has_value())
            {
                if(result.has_value())
                {
                    if(!result.get().is_nil())
                    {
                        throw MessageFormatException(STRCATS(
                            "For message of type " <<
                            printType(type) << ", result should be nil " <<
                            "if an error occurred but result == " <<
                            printOptional(result)));
                    }
                }
            }
            break;

        case Notification:
            if(result.has_value())
            {
                resultError();
            }
            break;
    }

}

BUFSTACK_END_NAMESPACE
