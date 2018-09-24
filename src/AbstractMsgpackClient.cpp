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


void AbstractMsgpackClient::startListening(int readFd, 
        std::function<void(MsgpackFdReader&)> f)
{
    std::lock_guard<std::mutex> {fdReaderMutex};
    if(!fdReader)
    {
        const MsgpackReaderUnpacker::Callback cb = 
                    std::bind(&AbstractMsgpackClient::onDecodeCallback, this,
                        std::placeholders::_1);

        fdReader = make_unique<MsgpackFdReader>(readFd, cb);

        getLogger()->debug(STRCATS("Listening on " << readFd));
        f(*fdReader);
    }
    else
    {
        throw AlreadyListeningException(STRCATS("Cannot listen on fd " <<
                    readFd << "; already listening on fd " << 
                    fdReader->getFd()));
    }
}

void AbstractMsgpackClient::startListening(int readFd)
{
    startListening(readFd, [](MsgpackFdReader& r) { r.startListening(); });
}

void AbstractMsgpackClient::asyncStartListening(int readFd)
{
    startListening(readFd, [](MsgpackFdReader& r) { r.asyncStartListening(); });
}

void AbstractMsgpackClient::addResponseCallback(BoundResponseCallback&& cb)
{
    std::lock_guard<decltype(callMutex)> {callMutex};

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
    std::lock_guard<decltype(callMutex)> {callMutex};

    responseCallbacks.access<void>(
        [&msg](std::vector<BoundResponseCallback>& callbacks){
            std::remove_if(callbacks.begin(), callbacks.end(),
                [&msg](BoundResponseCallback& thisCallback){
                    return thisCallback(msg);
                });
        });
}

AbstractMsgpackClient::~AbstractMsgpackClient()
{
    fdReader->join();
}



BUFSTACK_END_NAMESPACE
