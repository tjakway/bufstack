#include <gtest/gtest.h>

#include "Util/Util.hpp"

#include "NamespaceDefines.hpp"

namespace {
    const char delimiter = ':';
    std::string delimiterStr = std::string({delimiter});

    void splitStringTest(std::vector<std::string> expected,
            std::string input)
    {
        ASSERT_EQ(expected, 
            Util::splitString(input, delimiter));
    }
}

BUFSTACK_BEGIN_NAMESPACE


TEST(Util_TestSplitString, OnlyDelimiter)
{
    splitStringTest({}, std::string({delimiter}));
}

TEST(Util_TestSplitString, TestTwoItems)
{
    std::string left = "left",
        right = "right";
    splitStringTest({left, right}, 
            left + delimiterStr + right);
}


BUFSTACK_END_NAMESPACE
