#include "Server.hpp"

#include "NamespaceDefines.hpp"
#include "Util/Strcat.hpp"
#include "Util/Util.hpp"


#include <msgpack.hpp>
#include <rpc/client.h>

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

//namespace {

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
//}

BUFSTACK_BEGIN_NAMESPACE


void Server::readFd(int fd, 
        std::function<void(const std::vector<msgpack::object_handle>&)> callback)
{
    std::unique_ptr<char[]> buf 
        = std::unique_ptr<char[]>(new char[BUFFER_READ_SIZE]);

    std::vector<char> data;

    int amtRead = -1;

    while(amtRead != 0 && !done.load())
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
            throw SocketError(STRCAT("Error in ", __func__, ": ", strerror(errno)));
        }

        //try and decode this message, recording how much we found
        char* unconsumedBuffer;
        std::size_t amountNotConsumed = -1;
        std::vector<msgpack::object_handle> handles = 
            decode(data.data(), data.size(),
                    &unconsumedBuffer, &amountNotConsumed, *this);

        //invoke the callback if we decoded anything
        if(!handles.empty())
        {
            callback(handles);
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

void Server::interrupt()
{
    done.store(true);
}
bool Server::interrupted()
{
    return done.load();
}


//TODO: add to Server
void connect(const std::string& addr, uint16_t port)
{
    rpc::client nvimClient(addr, port);
}


BUFSTACK_END_NAMESPACE
