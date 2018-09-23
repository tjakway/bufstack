#include "MsgpackReaderUnpacker.hpp"

#include "NamespaceDefines.hpp"
#include "Util/Strcat.hpp"
#include "Util/Util.hpp"
#include "SocketException.hpp"
#include "Util/MsgpackUtil.hpp"


#include <msgpack.hpp>

#include <iterator>
#include <string>
#include <memory>
#include <vector>
#include <algorithm>
#include <chrono>
#include <thread>
#include <functional>
#include <deque>
#include <utility>

#include <cstdint>
#include <cstdlib>
#include <cassert>
#include <ctime>
#include <cerrno>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

//how much to read at once
#define BUFFER_READ_SIZE 65536

namespace {

//NOTE: can't read directly into the unpacker buffer because we might need to 
//include data in it from the last read
//returns the unconsumed buffer via a pointer into buf 
//thus, ***THE LIFETIME OF THE UNCONSUMED BUFFER IS THE SAME AS THE ORIGINAL BUF***
std::vector<msgpack::object_handle> decode(
        char* buf,
        std::size_t length,
        char** unconsumedBuffer,
        std::size_t* amountNotConsumed,
        bufstack::Loggable& log)
{

    //copy the buffer we read into the unpacker so it can use it
    msgpack::unpacker unpacker;
    unpacker.reserve_buffer(length);
    memcpy(unpacker.buffer(), buf, length);
    unpacker.buffer_consumed(length);

    std::vector<msgpack::object_handle> handles;

    //decode as many objects as we can, record how much memory they take up,
    //then return the handles to the objects in that memory region
    bool decodeDone = false;
    while(!decodeDone)
    {
        try
        {
            msgpack::object_handle oh;
            if(!unpacker.next(oh))
            {
                decodeDone = true;
            }
            else
            {
                handles.emplace_back(std::move(oh));
            }
        }
        catch(msgpack::type_error& e)
        {
            const time_t now = std::chrono::system_clock::to_time_t(
                            std::chrono::system_clock::now());
            log.getLogger()->warn(
                "failed to decode msgpack object at {};\tException info: {}",
                    ctime(&now), e.what());

            throw e;
        }
    }

    *unconsumedBuffer = unpacker.nonparsed_buffer();
    *amountNotConsumed = unpacker.nonparsed_size();
    return handles;
}

template <typename T>
void chronoToTimeval(T t, struct timeval* tv)
{
    assert(tv != nullptr);
    std::chrono::nanoseconds ns = 
        std::chrono::duration_cast<std::chrono::nanoseconds>(t);
    tv->tv_sec = 0;
    tv->tv_usec = ns.count();
}

template <typename T>
class ReadDecoder : public BUFSTACK_NS::Loggable
{
    std::vector<char> data;
    const T sleepInterval;
    std::unique_ptr<char[]> buf;

public:
    ReadDecoder(T _sleepInterval)
        : Loggable("ReadDecoder"),
        sleepInterval(_sleepInterval)
    {
        buf = std::unique_ptr<char[]>(new char[BUFFER_READ_SIZE]);
    }

    virtual ~ReadDecoder() {}

    int doRead(int fd, BUFSTACK_NS::MsgpackReaderUnpacker::Callback& cb)
    {
        assert(buf);

        int amtRead = read(fd, buf.get(), BUFFER_READ_SIZE);
        if(amtRead == EAGAIN || amtRead == EWOULDBLOCK)
        {
            std::this_thread::sleep_for(sleepInterval);
        }
        //if we read data copy into the result buffer
        else if(amtRead > 0)
        {
            data.reserve(data.size() + amtRead);
            std::copy_n(buf.get(), amtRead, std::back_inserter(data));
        }
        else if(amtRead < 0)
        {
            throw BUFSTACK_NS::MsgpackReaderUnpacker::ReadException(
                    STRCAT("Error in ", __func__, ": ", strerror(errno)));
        }
        //read returns 0 for end of file
        else
        {
            getLogger()->debug("end of file");
        }

        //don't bother decoding empty messages
        if(amtRead > 0)
        {
            getLogger()->debug(STRCATS("read data: " << 
                        Util::printVector(data)));

            //try and decode this message, recording how much we found
            char* unconsumedBuffer;
            std::size_t amountNotConsumed = -1;
            const std::vector<msgpack::object_handle> handles = 
                decode(data.data(), data.size(),
                        &unconsumedBuffer, &amountNotConsumed, *this);

            //invoke the callback if we decoded anything
            if(!handles.empty())
            {
                std::vector<std::reference_wrapper<const msgpack::object>> vs;
                vs.reserve(handles.size());
                
                for(auto& h : handles)
                {
                    vs.emplace_back(std::ref(h.get()));
                }

                cb(vs);
            }

            //if there was any data left over make sure we append any subsequent messages
            //to it
            if(amountNotConsumed > 0)
            {
                data = std::vector<char>(unconsumedBuffer, 
                        unconsumedBuffer + amountNotConsumed);
            }
        }

        return amtRead;
    }
};

}

BUFSTACK_BEGIN_NAMESPACE


void MsgpackReaderUnpacker::readFd(int fd, 
        Callback callback)
{
    assert(fd >= 0);
    assert(callback);

    int amtRead = -1;
    ReadDecoder<SleepIntervalType> reader(sleepInterval);

    //read returns 0 to indicate end of file
    //stop reading if that happens
    while(amtRead != 0 && !interrupted())
    {
        //NOTE: **DO NOT** move fd_set initialization code out of the loop
        //from select(2):
        //   On  exit,  each  of the file descriptor sets is modified in place to indicate which file descriptors actually changed status.
        //   (Thus, if using select() within a loop, the sets must be reinitialized before each call.)
        fd_set selectFds;
        struct timeval tv;


        //nfd is ***NOT*** number of file descriptors
        //from select(2) again:
        //
        //   nfds  should be set to the highest-numbered file descriptor in any of the three sets, plus 1.  The indicated file descriptors
        //   in each set are checked, up to this limit (but see BUGS).
        //
        //see https://unix.stackexchange.com/questions/7742/whats-the-purpose-of-the-first-argument-to-select-system-call
        //for why this is so
        const int nfd = fd + 1;

        //set our sleep interval
        chronoToTimeval<SleepIntervalType>(sleepInterval, &tv);

        const int numSelectFds = 1;

        FD_ZERO(&selectFds);
        FD_SET(fd, &selectFds);
        int selectRes = select(nfd, &selectFds, nullptr, nullptr, &tv);
        if(selectRes == -1)
        {
            auto _errno = errno;
            throw SelectException(
                    STRCAT("Error in select(2): " << strerror(_errno)));
        }
        else
        {
            //since select returns the number of changed file descriptors
            //it should never be greater than the number of file descriptors 
            //we passed in the first place
            assert(selectRes <= numSelectFds);

            //check if we can read
            if(FD_ISSET(fd, &selectFds))
            {
                amtRead = reader.doRead(fd, callback);
            }
        }
    }

}

MsgpackReaderUnpacker::MsgpackReaderUnpacker(
        std::size_t _backlogSize,
        std::chrono::milliseconds _sleepInterval)
    : backlogSize(_backlogSize),
    sleepInterval(_sleepInterval)
{}

MsgpackReaderUnpacker::MsgpackReaderUnpacker()
    : MsgpackReaderUnpacker(
            Config::Defaults::defaultBacklogSize, 
            Config::Defaults::serverSleepInterval)
{}

MsgpackReaderUnpacker::~MsgpackReaderUnpacker()
{
    done();
}

void MsgpackReaderUnpacker::done()
{
    interrupt();
}

BUFSTACK_END_NAMESPACE
