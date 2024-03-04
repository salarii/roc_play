cmake_minimum_required(VERSION 3.10)
project(MyRocProject)
include_directories(/usr/include/libhackrf/)
if(1)

    cmake_minimum_required(VERSION 2.6)
    set(CMAKE_VERBOSE_MAKEFILE ON)
    Project(connectApp)

    #SET(GCC_COMPILE_FLAGS " -O0 -g  ")
    SET(GCC_COMPILE_FLAGS " -O3  ")

    SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COMPILE_FLAGS}")
    SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ")


    add_executable(app  platform/platform.cpp  platform/hackRF.cpp platform/audio.cpp platform/flow.cpp)
    target_link_libraries(app PRIVATE  portaudio pthread hackrf stdc++ GL GLEW glfw )
    target_compile_features(app PRIVATE cxx_std_17)

    #target_link_libraries(testApp PRIVATE  portaudio pthread hackrf stdc++ GL GLEW glfw )

else()


    # Set C standard
    set(CMAKE_C_STANDARD 99)
    # Ensure C++ is also enabled for the project if you have C++ sources
    enable_language(CXX)
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_VERBOSE_MAKEFILE ON)
    # Compile the C source file into an object file
    add_library(hackrf_platform OBJECT platform/host.c platform/flow.cpp)

    # Add a custom command to run the external roc application
    # Assuming this generates 'app.o' in the current binary directory
    add_custom_command(
        OUTPUT app.o
        COMMAND roc build --no-link ${CMAKE_SOURCE_DIR}/app.roc || echo "Warning or error ignored"
        DEPENDS ${CMAKE_SOURCE_DIR}/app.roc
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating app.o from app.roc"
    )

    if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
    endif()

    # Add definitions for both C and C++ compilers
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")


    add_compile_options(-w)
    # Create an executable target without specifying any sources
    add_executable(app app.o)
    # If you have libraries to link against (e.g., -lm for math), specify them here
    target_link_libraries(app PRIVATE hackrf_platform m)



endif()