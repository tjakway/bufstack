#include "Server.hpp"

#include "NamespaceDefines.hpp"

#include "Util/Strcat.hpp"

BUFSTACK_BEGIN_NAMESPACE

void MsgpackServer::onRecvMsg(const msgpack::object& o)
{
    std::vector<msgpack::object> msgObj;

    if(o.type != msgpack::type::object_type::ARRAY)
    {
        //TODO: throw exception
        throw NotMessageError(STRCATS(
            "Expected msgpack object of type " <<
            "ARRAY but got type code " << o.type));
    }
    else
    {
        o.convert(msgObj);

    }
}

BUFSTACK_END_NAMESPACE
