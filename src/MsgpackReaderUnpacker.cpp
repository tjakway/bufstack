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
#include <ctime> //for ctime()

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
}

BUFSTACK_BEGIN_NAMESPACE


void MsgpackReaderUnpacker::readFd(int fd, 
        std::function<void(const ObjectList&)> callback)
{
    assert(fd >= 0);
    assert(callback);

    std::unique_ptr<char[]> buf 
        = std::unique_ptr<char[]>(new char[BUFFER_READ_SIZE]);

    std::vector<char> data;

    int amtRead = -1;

    while(amtRead != 0 && !interrupted())
    {
        amtRead = read(fd, buf.get(), BUFFER_READ_SIZE);
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
        else if(amtRead != 0)
        {
            throw SocketException(STRCAT("Error in ", __func__, ": ", strerror(errno)));
        }

        getLogger()->debug(STRCATS("data: " << Util::printVector(data)));

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

            callback(vs);
        }

        //if there was any data left over make sure we prepend any subsequent messages
        //to it
        if(amountNotConsumed > 0)
        {
            data = std::vector<char>(unconsumedBuffer, 
                    unconsumedBuffer + amountNotConsumed);
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
