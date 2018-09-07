#include "Util/MsgpackUtil.hpp"

#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

std::vector<std::reference_wrapper<const msgpack::object>> 
        MsgpackUtil::wrapObjects(const std::vector<msgpack::object>& v)
{
    return std::vector<std::reference_wrapper<const msgpack::object>>(v.begin(), v.end()) ;
}

BUFSTACK_END_NAMESPACE
