#include <sstream>
#include <msgpack.hpp>

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

template <typename T>
class MsgpackTestObject
{
public:
    T original;
    msgpack::sbuffer buf;
    msgpack::object_handle obj;

    MsgpackTestObject(T _original)
        : original(_original)
    {
        //msgpack::packer<msgpack::sbuffer> p(buf);
        //p.pack(original);
        msgpack::pack(buf, original);

        obj = msgpack::unpack(buf.data(), buf.size());

        T x;
        obj.get().convert(x);
        assert(x == original);
    }

};

/*
template <>
class MsgpackTestObject<std::string>
{
public:
    std::string original;
    std::stringstream buf;
    msgpack::object_handle obj;

    MsgpackTestObject(std::string _original)
        : original(_original)
    {
        msgpack::packer<std::stringstream> p(buf);
        p.pack_str(original.size());
        p.pack_str_body(original.c_str(), original.size());

        std::string bufStr(buf.str());
        obj = msgpack::unpack(bufStr.data(), bufStr.size());
    }
}; */

BUFSTACK_END_NAMESPACE
