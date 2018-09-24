#include "MsgpackFdReader.hpp"

#include <functional>
#include <string>
#include <cerrno>

#include <unistd.h>
#include <fcntl.h>

#include "Util/Strcat.hpp"

namespace {

    void setFdNonblocking(int fd,
        std::function<void(const std::string&)> onError,
        BUFSTACK_NS::Loggable& log)
    {
        //save current flags, OR with O_NONBLOCK
        int flags = fcntl(fd, F_GETFL, 0);
        if (flags == -1)
        {
            auto _errno = errno;
            onError(STRCAT("fcntl returned -1 for F_SETFL in " <<
                        __func__ << ": " <<
                        strerror(_errno)));
        }

        if((flags & O_NONBLOCK) == 0)
        {
            flags |= O_NONBLOCK;
            if(fcntl(fd, F_SETFL, flags) == -1)
            {
                auto _errno = errno;
                onError(STRCAT("fcntl returned -1 for F_SETFL in " <<
                            __func__ << ": " <<
                            strerror(_errno)));
            }

            log.getLogger()->debug(STRCATS("set fd " << fd <<
                        " to nonblocking"));
        }
        else
        {
            log.getLogger()->debug(STRCATS("fd " << fd <<
                        " is already nonblocking"));
        }
    }
}


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
    setFdNonblocking(fd, [](const std::string& msg) {
        throw SetFdNonblockingException(msg);
    }, *this);

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
                    this->listening.store(true);
                    this->readFd(this->fd, this->cb);
                }

                //done listening
                this->listening.store(false);
            };

        listenThread = make_unique<std::thread>(threadCb);

        //wait until the thread is running before returning
        while(!threadInitialized.load())
        {
            getLogger()->debug(STRCATS(
                "Async listener thread not up yet, sleeping for " << 
                spinInterval.count() << " milliseconds"));
            std::this_thread::sleep_for(spinInterval);
        }
    }
}

BUFSTACK_END_NAMESPACE
