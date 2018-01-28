#include <sstream>
#include <msgpack.hpp>

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

template <typename T>
class MsgpackTestObject
{
public:
    T expected;
    std::stringstream buf;
    msgpack::object_handle obj;

    MsgpackTestObject(T _expected)
        : expected(_expected)
    {
        msgpack::pack(buf, expected);

        std::string bufStr(buf.str());
        obj = msgpack::unpack(bufStr.data(), bufStr.size());
    }
};

BUFSTACK_END_NAMESPACE
