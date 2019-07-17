cmake_minimum_required(VERSION 3.0)

project(ADL_RUNTIME C CXX)

add_subdirectory(3rd-party)

include_directories(
  ./include
  ./3rd-party/libb64-1.2/include
  ./src-generated
)

add_library(adl_runtime
  src/adl.cpp
  src/json.cpp
  src/jsonimpl.cpp
  src/types.cpp
  src-generated/sys.adlast.cpp
  src-generated/sys.types.cpp
)

target_link_libraries(adl_runtime
  b64
)
