#pragma once

#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

class AbstractRemoteApiFunction;

template <typename T, typename... Args>
class HasReturnValueRemoteApiFunction;

template <typename T, typename... Args>
class NoReturnRemoteApiFunction;

class RemoteFunctionInstances;

BUFSTACK_END_NAMESPACE
