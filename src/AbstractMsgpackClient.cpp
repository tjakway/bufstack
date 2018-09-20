#include "AbstractMsgpackClient.hpp"

#include "Util/AtomicAccess.hpp"

#include <algorithm>
#include <utility>

BUFSTACK_BEGIN_NAMESPACE

void AbstractMsgpackClient::onDecodeCallback(
        const MsgpackReaderUnpacker::ObjectList& objects)
{
    for(auto i : objects)
    {
        onRecvMsg(i.get());
    }
}


void AbstractMsgpackClient::startListening(int readFd)
{
    std::lock_guard<std::mutex> {fdReaderMutex};
    if(!fdReader)
    {
        const MsgpackReaderUnpacker::Callback cb = 
                    std::bind(&AbstractMsgpackClient::onDecodeCallback, this,
                        std::placeholders::_1);

        fdReader = make_unique<MsgpackFdReader>(readFd, cb);

        getLogger()->debug(STRCATS("Listening on " << readFd));
        fdReader->startListening();
    }
    else
    {
        throw AlreadyListeningException(STRCATS("Cannot listen on fd " <<
                    readFd << "; already listening on fd " << 
                    fdReader->getFd()));
    }
}

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
                    return thisCallback(msg);
                });
        });
}

BUFSTACK_END_NAMESPACE
