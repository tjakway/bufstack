#include "HasTcpConnection.hpp"

#include "Util/Util.hpp"
#include "Util/Strcat.hpp"

#include <string>

BUFSTACK_BEGIN_NAMESPACE

constexpr const char HasTcpConnection::localhost[];

void HasTcpConnection::decodeAddress(
        const std::string& address, in_addr* addr)
{
    if(Util::StringTrim::trim_copy(address).empty())
    {
        throw BadAddressException("Passed address is empty");
    }
    else if(addr == nullptr)
    {
        throw BadAddressException(
            STRCATS("in_addr* is null for call to " <<
                __func__  << " with address " << address));
    }
    else if(inet_aton(address.c_str(), addr) != 1)
    {
        throw BadAddressException(STRCATS(
                    "Could not interpret " <<
                    address << " as an IPv4" <<
                    " address"));
    }
}

BUFSTACK_END_NAMESPACE
