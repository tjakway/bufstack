#include "MsgpackFdReader.hpp"


BUFSTACK_BEGIN_NAMESPACE

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        std::size_t _backlogSize,
        std::chrono::milliseconds _sleepInterval)
: Loggable("MsgpackFdReader"),
    MsgpackReaderUnpacker(_backlogSize, _sleepInterval), 
    fd(_fd)
{}

void MsgpackFdReader::startListening()
{
    while(!interrupted())
    {
        readFd(_fd, cb);
    }
}

BUFSTACK_END_NAMESPACE
