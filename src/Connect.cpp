#include "Connect.hpp"

msgpack::object_handle Connect::getApiInfo()
{
    return client->call("vim_get_api_info");
}
