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

set(SCHEME_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/game.ss")
set(BOOT_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/game.boot")
set(GAME_BOOT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/game_boot.h")
set(PETITE_BOOT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/petite_boot.h")
set(SCHEME_BOOT_HEADER "${CMAKE_CURRENT_BINARY_DIR}/scheme_boot.h")
set(PETITE_BOOT "${CHEZ_INSTALL_DIR}/ta6le/petite.boot")
set(SCHEME_BOOT "${CHEZ_INSTALL_DIR}/ta6le/scheme.boot")

# Find Chez Scheme executable (assumed to be 'scheme' or 'petite')
find_program(CHEZ_SCHEME_EXE NAMES scheme petite REQUIRED)

# Custom command to compile .ss -> .boot -> C Header
add_custom_command(
    OUTPUT "${GAME_BOOT_HEADER}"
    COMMAND "${CHEZ_SCHEME_EXE}" --script "${CMAKE_CURRENT_SOURCE_DIR}/scripts/make_boot.ss" "${SCHEME_SOURCE}" "${BOOT_OUTPUT}"
    COMMAND cmake -E echo "Converting game.boot to C header..."
    # Using 'xxd -i' to convert binary to C array
    COMMAND xxd -i "game.boot" "${GAME_BOOT_HEADER}"
    DEPENDS "${SCHEME_SOURCE}"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Building embedded Chez Scheme boot code"
)
add_custom_command(
    OUTPUT "${PETITE_BOOT_HEADER}"
    COMMAND cmake -E echo "Converting petite.boot to C header..."
    COMMAND xxd -i -n petite_boot "${PETITE_BOOT}" "${PETITE_BOOT_HEADER}"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Converting petite.boot into header"
)
add_custom_command(
    OUTPUT "${SCHEME_BOOT_HEADER}"
    COMMAND cmake -E echo "Converting scheme.boot to C header..."
    COMMAND xxd -i -n scheme_boot "${SCHEME_BOOT}" "${SCHEME_BOOT_HEADER}"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Converting scheme.boot into header"
)

add_executable(chez-invaders)
target_include_directories(chez-invaders PRIVATE ${PROJECT_INCLUDE})
target_link_libraries(chez-invaders PRIVATE raylib)
target_link_libraries(chez-invaders PRIVATE
    "${CHEZ_INSTALL_DIR}/ta6le/libkernel.a"
    "${CHEZ_INSTALL_DIR}/ta6le/libz.a"
    "${CHEZ_INSTALL_DIR}/ta6le/liblz4.a"
    uuid m dl rt pthread ncurses # Core OS/Chez dependencies depending on build
)
target_compile_definitions(chez-invaders PRIVATE $<$<CONFIG:Release>:RELEASE>)
target_sources(chez-invaders PRIVATE main.c repl.c inotify_watcher.c
    ${GAME_BOOT_HEADER}
    ${PETITE_BOOT_HEADER}
    ${SCHEME_BOOT_HEADER})