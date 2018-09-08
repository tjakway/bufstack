#include "AbstractMsgpackClient.hpp"

#include "Util/AtomicAccess.hpp"

#include <algorithm>
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


void AbstractMsgpackClient::onReceiveResponseMsg(
        const MsgpackRpc::ResponseMessage& msg)
{
    responseCallbacks.access<void>(
        [&msg](std::vector<BoundResponseCallback>& callbacks){
            std::remove_if(callbacks.begin(), callbacks.end(),
                [&msg](BoundResponseCallback& thisCallback){
                    return thisCallback(msg.msgId);
                });
        });
}

BUFSTACK_END_NAMESPACE
