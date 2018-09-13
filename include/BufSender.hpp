#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"

#include "Buffer.hpp"
#include "Loggable.hpp"
#include "SocketException.hpp"

BUFSTACK_BEGIN_NAMESPACE

class BufSender : virtual public Loggable
{
protected:
    NEW_EXCEPTION_TYPE(BufSenderException);
    using BaseException = BufSenderException;

    static void sendAll(int, const char* buf, std::size_t bufLen, Loggable&);
public:
    virtual void send(int, Buffer) = 0;
    virtual void send(int, const char*, std::size_t) = 0;

    virtual ~BufSender() {}
};

BUFSTACK_END_NAMESPACE
