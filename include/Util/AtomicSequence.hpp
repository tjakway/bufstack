#pragma once

#include <mutex>
#include <memory>

template <typename T>
class AtomicSequence
{
    std::mutex mut;
    std::unique_ptr<T> seqPtr;

protected:
    T readNoIncrement()
    {
        return *seqPtr;
    }

public:
    T nextAndIncrement()
    {
        std::lock_guard<std::mutex> lock(mut);
        (*seqPtr)++;
        return *seqPtr;
    }
};

