#pragma once

#include "NamespaceDefines.hpp"

#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

class BufSender : virtual public Loggable
{
protected:
    static void sendAll(int, const char* buf, ssize_t bufLen, Loggable&);
public:
    virtual void send(int, Buffer) = 0;
    virtual void send(int, const char*, std::size_t) = 0;
};

BUFSTACK_END_NAMESPACE
