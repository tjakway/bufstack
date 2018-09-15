#include "MsgpackFdReader.hpp"


BUFSTACK_BEGIN_NAMESPACE

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode,
        std::size_t _backlogSize,
        std::chrono::milliseconds _sleepInterval)
: Loggable("MsgpackFdReader"),
    MsgpackReaderUnpacker(_backlogSize, _sleepInterval), 
    fd(_fd)
{}

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode)
: Loggable("MsgpackFdReader"),
    //call with default args
    MsgpackReaderUnpacker(), 
    fd(_fd)
{}

void MsgpackFdReader::startListening()
{
    while(!interrupted())
    {
        readFd(fd, cb);
    }
}

BUFSTACK_END_NAMESPACE
