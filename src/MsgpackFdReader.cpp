#include "MsgpackFdReader.hpp"


BUFSTACK_BEGIN_NAMESPACE

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode,
        std::size_t _backlogSize,
        std::chrono::milliseconds _sleepInterval)
: Loggable("MsgpackFdReader"),
    MsgpackReaderUnpacker(_backlogSize, _sleepInterval), 
    fd(_fd),
    listening(false)
{}

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode)
: Loggable("MsgpackFdReader"),
    //call with default args
    MsgpackReaderUnpacker(), 
    fd(_fd),
    listening(false)
{}

void MsgpackFdReader::startListening()
{
    //check that we're not already reading
    if(!listening)
    {
        while(!interrupted())
        {
            listening.store(true);
            readFd(fd, cb);
        }
        
        //done listening
        listening.store(false);
    }
}

BUFSTACK_END_NAMESPACE
