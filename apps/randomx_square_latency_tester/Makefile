# Compiler
CXX = g++

# Include directories
INCLUDES = -I../bigfile/c_src/randomx -I ../bigfile/lib/RandomX/src

# Compiler Flags
CXXFLAGS = -msse4.2 -mavx2 -Wall -O2 $(INCLUDES)

# Linker Flags
LDFLAGS = -L/usr/local/lib
LDLIBS = -lssl -lcrypto

# Source files in ../bigfile/c_src/randomx
DEPS_SOURCES := $(wildcard ../bigfile/c_src/randomx/*.cpp)
DEPS_OBJECTS := $(patsubst ../bigfile/c_src/randomx/%.cpp,%.o,$(DEPS_SOURCES))

# Local main.cpp
MAIN_SOURCES := main.cpp
MAIN_OBJECTS := $(patsubst %.cpp,%.o,$(MAIN_SOURCES))

# All object files
OBJECTS := $(DEPS_OBJECTS) $(MAIN_OBJECTS)

# Path to RandomX library
RANDOMX_LIB = ../bigfile/lib/RandomX/build4096/librandomx4096.a

# Target executable
TARGET = main

# Default target
all: $(TARGET)

# Link object files to create the executable
$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(RANDOMX_LIB) $(LDFLAGS) $(LDLIBS)

# Compile source files from ../bigfile/c_src/randomx
%.o: ../bigfile/c_src/randomx/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Compile local main.cpp
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean
