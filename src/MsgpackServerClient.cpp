#include "Server.hpp"

#include <functional>

BUFSTACK_BEGIN_NAMESPACE


void MsgpackServerClient::addResponseCallback(BoundResponseCallback&& cb)
{
    const auto f = [](std::vector<BoundResponseCallback>& v, 
            BoundResponseCallback&& callback) -> void
    {
        v.emplace_back(callback);
    };

    responseCallbacks.access(
            std::bind(f, std::placeholders::_1, std::move(cb)));
}

BUFSTACK_END_NAMESPACE
