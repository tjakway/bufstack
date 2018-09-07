#include "AbstractMsgpackClient.hpp"

#include <utility>

BUFSTACK_BEGIN_NAMESPACE

void AbstractMsgpackClient::addResponseCallback(BoundResponseCallback&& cb)
{
    std::function<void(std::vector<BoundResponseCallback>&, 
            BoundResponseCallback&&)> f = 
        [](std::vector<BoundResponseCallback>& v, 
            BoundResponseCallback&& callback) -> void
    {
        v.emplace_back(callback);
    };

    auto g = [f, &cb](std::vector<BoundResponseCallback>& v){
        return f(v, std::move(cb));
    };
    responseCallbacks.access<void>(g);
}

BUFSTACK_END_NAMESPACE