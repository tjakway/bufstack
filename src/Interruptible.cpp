#include "Interruptible.hpp"

#include "NamespaceDefines.hpp"
BUFSTACK_BEGIN_NAMESPACE

/**
 * dtor stops the loop
 */
Interruptible::~Interruptible()
{
    interrupt();
}

BUFSTACK_END_NAMESPACE
