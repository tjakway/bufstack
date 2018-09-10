#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include <string>
#include <memory>
#include <vector>

BUFSTACK_BEGIN_NAMESPACE

class FindNeovim
{
    FindNeovim() = delete;

    static Loggable logger;

    /**
     * suppress warnings for this directory if dontWarn returns true
     */
    static bool dontWarn(const std::string&);
    static bool isDirectory(const std::string&);
    static std::vector<std::string> getPathEntries(Loggable&);
    static std::vector<std::string> getFilesInDirectory(const std::string&,
            Loggable&);
public:
    NEW_EXCEPTION_TYPE(FindNeovimException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NoPathVariable, FindNeovimException);
    NEW_EXCEPTION_TYPE_WITH_BASE(DirectoryException, FindNeovimException);

    static const std::string neovimExeName;

    /**
     * find the first occurrence on the PATH variable
     */
    static std::unique_ptr<std::string> getFirstOnPath(std::string target = neovimExeName);
};

BUFSTACK_END_NAMESPACE
