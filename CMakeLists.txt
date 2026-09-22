cmake_minimum_required(VERSION 3.28)
project(chez-invaders)

include(FetchContent)
set(CHEZ_INSTALL_DIR "/usr/lib/csv10.4.1" CACHE PATH "Path to Chez Scheme installation")

set(FETCHCONTENT_QUIET FALSE)
set(BUILD_EXAMPLES OFF CACHE BOOL "" FORCE) # don't build the supplied examples
set(BUILD_GAMES    OFF CACHE BOOL "" FORCE) # don't build the supplied example games
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

FetchContent_Declare(
  raylib
  GIT_REPOSITORY "https://github.com/raysan5/raylib.git"
  GIT_TAG dbc56a87da87d973a9c5baa4e7438a9d20121d28 # v6.0
)

# Save your original global setting
set(OLD_BUILD_SHARED ${BUILD_SHARED_LIBS})
# Force shared ONLY for raylib
set(BUILD_SHARED_LIBS ON CACHE INTERNAL "" FORCE)
FetchContent_MakeAvailable(raylib)
# Restore your original setting for the rest of your C files / Chez targets
set(BUILD_SHARED_LIBS ${OLD_BUILD_SHARED} CACHE INTERNAL "" FORCE)

set(PROJECT_INCLUDE "${CMAKE_CURRENT_LIST_DIR}/sources/") # Define PROJECT_INCLUDE to be the path to the include directory of the project

add_executable(chez-invaders)
target_include_directories(chez-invaders PRIVATE ${PROJECT_INCLUDE})
target_link_libraries(chez-invaders PRIVATE raylib)
target_link_libraries(chez-invaders PRIVATE
    "${CHEZ_INSTALL_DIR}/ta6le/libkernel.a"
    "${CHEZ_INSTALL_DIR}/ta6le/libz.a"
    "${CHEZ_INSTALL_DIR}/ta6le/liblz4.a"
    uuid m dl rt pthread ncurses # Core OS/Chez dependencies depending on build
)
target_sources(chez-invaders PRIVATE main.c)