#include "AsyncBufSender.hpp"

#include <algorithm>

BUFSTACK_BEGIN_NAMESPACE

void AsyncBufSender::reapFutures() noexcept
{
    futures.erase(std::remove_if(futures.begin(),
        futures.end(), [](FutureType f){
            return (!f.valid()) || 
            //see https://stackoverflow.com/questions/10890242/get-the-status-of-a-stdfuture
            //re: checking if a future is ready
                (f.wait_for(std::chrono::seconds(0)) == 
                    std::future_status::ready);
        }));
}

BUFSTACK_END_NAMESPACE
