#include "MsgpackFdReader.hpp"


BUFSTACK_BEGIN_NAMESPACE


const std::chrono::milliseconds MsgpackFdReader::spinInterval =
    std::chrono::milliseconds(10);

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode,
        std::size_t _backlogSize,
        std::chrono::milliseconds _sleepInterval)
: Loggable("MsgpackFdReader"),
    MsgpackReaderUnpacker(_backlogSize, _sleepInterval), 
    fd(_fd),
    cb(onDecode),
    listening(false)
{}

MsgpackFdReader::MsgpackFdReader(
        int _fd,
        Callback onDecode)
: Loggable("MsgpackFdReader"),
    //call with default args
    MsgpackReaderUnpacker(), 
    fd(_fd),
    cb(onDecode),
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

MsgpackFdReader::~MsgpackFdReader()
{
    if(listenThread)
    {
        //signal to the thread to stop
        //and wait for it to finish
        interrupt();
        if(listenThread->joinable())
        {
            listenThread->join();
        }
    }
}

void MsgpackFdReader::asyncStartListening()
{
    //check that we're not already reading
    if(!listening)
    {
        std::atomic_bool threadInitialized{false};

        std::function<void(void)> threadCb = 
            [this, &threadInitialized]() {

                //set the flag that indicates we've
                //begun execution
                threadInitialized.store(true);

                while(!this->interrupted())
                {
                    listening.store(true);
                    this->readFd(this->fd, this->cb);
                }

                //done listening
                listening.store(false);
            };

        listenThread = make_unique<std::thread>(threadCb);

        //wait until the thread is running before returning
        while(!threadInitialized.load())
        {
            std::this_thread::sleep_for(spinInterval);
        }
    }
}

BUFSTACK_END_NAMESPACE
