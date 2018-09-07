#pragma once

#include "NamespaceDefines.hpp"
#include "HasFd.hpp"

BUFSTACK_BEGIN_NAMESPACE

class MsgpackClient : 
    protected AbstractMsgpackClient,
    virtual public HasClientFd
{


public:
    MsgpackClient(
        const std::string& address,
        uint16_t port);

    virtual ~MsgpackClient() {}

    NEW_EXCEPTION_TYPE_WITH_BASE(ConnectionException, BaseException);
    NEW_EXCEPTION_TYPE_WITH_BASE(BadAddressException, ConnectionException);
};

BUFSTACK_END_NAMESPACE
