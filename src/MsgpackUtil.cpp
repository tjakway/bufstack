#include "Util/MsgpackUtil.hpp"

#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

std::vector<std::reference_wrapper<const msgpack::object>> 
        MsgpackUtil::wrapObjects(const std::vector<msgpack::object>& v)
{
    return std::vector<std::reference_wrapper<const msgpack::object>>(v.begin(), v.end()) ;
}

/**
 * taken from msgpack-c/include/msgpack/v1/object.hpp (in namespace detail)
 * see MsgpackUtil.cpp header regarding license
 */
msgpack::object_handle MsgpackUtil::clone(msgpack::object const& obj) 
{
    std::size_t size = msgpack::aligned_zone_size(obj);
    msgpack::unique_ptr<msgpack::zone> z(size == 0 ? MSGPACK_NULLPTR : new msgpack::zone(size));
    msgpack::object newobj = z.get() ? msgpack::object(obj, *z) : obj;
    return msgpack::object_handle(newobj, msgpack::move(z));
}

BUFSTACK_END_NAMESPACE
