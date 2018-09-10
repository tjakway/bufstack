#pragma once

#include "NamespaceDefines.hpp"
#include "Util/NewExceptionType.hpp"
#include "Loggable.hpp"

#include <string>
#include <memory>
#include <vector>

BUFSTACK_BEGIN_NAMESPACE

class FindNeovim
    : public Loggable
{
    /**
     * suppress warnings for this directory if dontWarn returns true
     */
    bool dontWarn(const std::string&);
    bool skipEntry(const std::string&);
    bool isDirectory(const std::string&);
    std::vector<std::string> getPathEntries();
    std::vector<std::string> getFilesInDirectory(const std::string&);
public:
    NEW_EXCEPTION_TYPE(FindNeovimException);
    NEW_EXCEPTION_TYPE_WITH_BASE(NoPathVariable, FindNeovimException);
    NEW_EXCEPTION_TYPE_WITH_BASE(DirectoryException, FindNeovimException);

    static constexpr auto defaultNeovimExeName = "nvim";
    static constexpr auto defaultPathVarName = "PATH";

    const std::string pathVarName; 
    const std::string neovimExeName;

    /**
     * find the first occurrence on the PATH variable
     */
    std::unique_ptr<std::string> getFirstOnPath();

    FindNeovim(std::string _neovimExeName = defaultNeovimExeName,
            std::string _pathVarName = defaultPathVarName)
        : Loggable("FindNeovim"),
        neovimExeName(_neovimExeName),
        pathVarName(_pathVarName)
    {}

    virtual ~FindNeovim() {}
};

BUFSTACK_END_NAMESPACE
