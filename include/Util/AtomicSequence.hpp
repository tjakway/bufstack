#pragma once

#include <mutex>
#include <memory>

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

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
    AtomicSequence(T startingValue)
        : seqPtr(new T(startingValue))
    {}

    AtomicSequence()
        : seqPtr(new T())
    {}

    T nextAndIncrement()
    {
        std::lock_guard<std::mutex> lock(mut);
        (*seqPtr) += 1;
        return *seqPtr;
    }
};


BUFSTACK_END_NAMESPACE
