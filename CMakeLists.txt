cmake_minimum_required(VERSION 3.20)
project(MyTidyPlugin CXX)
set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

include(GNUInstallDirs)
set(PLUGIN_INSTALL_DIR
    "${CMAKE_INSTALL_LIBDIR}/clang-tidy-plugins"
    CACHE PATH "Install directory for clang-tidy plugins")

add_library(elysium-tidy MODULE plugin.cpp)

target_include_directories(elysium-tidy PRIVATE
  ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

target_link_libraries(elysium-tidy PRIVATE
  clangTidy
  clangTooling
  clangASTMatchers
  clangAST
  clangBasic
  clangFrontend
  clang-cpp)

install(TARGETS elysium-tidy
  LIBRARY DESTINATION "${PLUGIN_INSTALL_DIR}"        # .so/.dylib
  RUNTIME DESTINATION "${PLUGIN_INSTALL_DIR}"        # .dll (Windows)
  ARCHIVE DESTINATION "${PLUGIN_INSTALL_DIR}")       # import lib, if any

message(STATUS "Plugin will install to: ${PLUGIN_INSTALL_DIR}")
