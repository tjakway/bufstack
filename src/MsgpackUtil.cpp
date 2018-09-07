#include "Util/MsgpackUtil.hpp"

#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

std::vector<std::reference_wrapper<msgpack::object>> 
        MsgpackUtil::wrapObjects(const std::vector<msgpack::object>& v)
{
    std::vector<std::reference_wrapper<msgpack::object>> refs;
    refs.reserve(v.size());

    std::transform(v.begin(), v.end(), refs.begin(),
        [](const msgpack::object& o) {
            return std::ref(o);
        });

    return refs;
}

BUFSTACK_END_NAMESPACE
