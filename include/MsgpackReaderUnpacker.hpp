#pragma once

#include <utility>
#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "HasFd.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackReaderUnpacker : 
    virtual public HasClientFd
{
public:
    using ObjectQueue = std::deque<std::reference_wrapper<msgpack::object>>;
    using ObjectList = std::vector<std::reference_wrapper<msgpack::object>>;

protected:
    virtual void readFd(int, 
            std::function<void(const ObjectList&)>);


public:
    MsgpackReaderUnpacker() {}
    virtual ~MsgpackReaderUnpacker() {}
};

BUFSTACK_END_NAMESPACE
