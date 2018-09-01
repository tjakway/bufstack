#include <gtest/gtest.h>

#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

class RemoteFunctionTests : public ::testing::Test, public Loggable
{
public:
    RemoteFunctionTests()
        : Loggable("RemoteFunctionTests")
    {}
};

BUFSTACK_END_NAMESPACE
