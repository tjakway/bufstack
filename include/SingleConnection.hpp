#pragma once

#include "Connectible.hpp"

#include "Util/NewExceptionType.hpp"
#include "NamespaceDefines.hpp"

BUFSTACK_BEGIN_NAMESPACE

/**
 * we only expect one neovim instance to connect at a time
 */
class SingleConnection : public Connectible
{
protected:
    NEW_EXCEPTION_TYPE_WITH_BASE(SingleConnectionException, BaseException);

    std::atomic_bool connected {false};
    virtual void onConnect() override;

public:
    virtual ~SingleConnection() {}
};

BUFSTACK_END_NAMESPACE
