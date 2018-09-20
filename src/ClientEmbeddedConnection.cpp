#include "ClientConnection.hpp"

#include <utility>
#include <string>

BUFSTACK_BEGIN_NAMESPACE


ClientEmbeddedConnection::ClientEmbeddedConnection(
        FdWrapper&& _readFd, FdWrapper&& _writeFd)
    : Loggable("ClientEmbeddedConnection"), 
    readFd(std::move(_readFd)), writeFd(std::move(_writeFd))
{}

ClientEmbeddedConnection::ClientEmbeddedConnection(
        ClientEmbeddedConnection&& other)

    : ClientEmbeddedConnection(
            std::move(other.readFd), std::move(other.writeFd))
{}


PrintableObject::Fields ClientEmbeddedConnection::getFields() const noexcept
{
    return Fields {
        std::make_pair("readFd", readFd.printCompact()),
        std::make_pair("writeFd", writeFd.printCompact())
    };
}

std::string ClientEmbeddedConnection::getName() const noexcept
{
    return "ClientEmbeddedConnection";
}

int ClientEmbeddedConnection::getWriteFd() const
{
    return writeFd.getFd();
}

int ClientEmbeddedConnection::getReadFd() const
{
    return readFd.getFd();
}

BUFSTACK_END_NAMESPACE
