#include <gtest/gtest.h>

#include <msgpack.hpp>

#include "NamespaceDefines.hpp"
#include "NvimConnectionTest.hpp"
#include "Loggable.hpp"

BUFSTACK_BEGIN_NAMESPACE

class RemoteFunctionTests 
    : public ::testing::Test, 
    public Loggable,
    public NvimConnectionTest
{
public:
    RemoteFunctionTests()
        : Loggable("RemoteFunctionTests")
    {}
};

TEST_F(RemoteFunctionTests, TestGetClientInstance)
{
    auto client = getClientInstance();
}

BUFSTACK_END_NAMESPACE
