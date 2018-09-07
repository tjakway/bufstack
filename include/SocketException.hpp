#pragma once

#include "NamespaceDefines.hpp"

#include "Util/NewExceptionType.hpp"

BUFSTACK_BEGIN_NAMESPACE

/**
 * thrown when an error occurs while reading or writing to a socket
 */
NEW_EXCEPTION_TYPE(SocketException);

BUFSTACK_END_NAMESPACE
