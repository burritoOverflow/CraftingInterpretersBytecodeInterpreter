# This Makefile leverages CMake to produce the several different build types.
# This gives the ability to call 'make' in the root of the project and get
# sensible results, which allows nice integration into vim's ':make' command,
# as well as aliases like make='bear make' for integration with YouCompleteMe.

PROJECTROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BUILDROOT := $(abspath $(PROJECTROOT)/BUILD)
.DEFAULT_GOAL := all
.PHONY : all
all : debug release relwithdebinfo

# use Ninja if present
CMAKE_GENERATOR ?= $(shell (command -v ninja > /dev/null 2>&1 && echo "Ninja") || \
    echo "Unix Makefiles")

CC = gcc
CLANGCHECK := $(shell which clang)
ifeq ($(CLANGCHECK),)
$(info CC - clang not found; using gcc)
else
$(info CC - clang found; using clang)
	CC = clang
endif

.PHONY : echo
echo :
	@echo "PROJECTROOT=$(PROJECTROOT)"
	@echo "BUILDROOT=$(BUILDROOT)"

.PHONY : clean
clean :
	$(RM) -r "$(BUILDROOT)"

.PHONY : debug
debug :
	cmake -S "$(PROJECTROOT)" -B "$(BUILDROOT)/Debug" -DCMAKE_C_COMPILER=$(CC) -DCMAKE_BUILD_TYPE=Debug -G $(CMAKE_GENERATOR)
	cmake --build "$(BUILDROOT)/Debug" --verbose

.PHONY : release
release :
	cmake -S "$(PROJECTROOT)" -B "$(BUILDROOT)/Release" -DCMAKE_C_COMPILER=$(CC)  -DCMAKE_BUILD_TYPE=Release -G $(CMAKE_GENERATOR)
	cmake --build "$(BUILDROOT)/Release" --verbose

.PHONY : RelWithDebInfo
relwithdebinfo :
	cmake -S "$(PROJECTROOT)" -B "$(BUILDROOT)/RelWithDebugInfo" -DCMAKE_C_COMPILER=$(CC) -DCMAKE_BUILD_TYPE=RelWithDebugInfo -G $(CMAKE_GENERATOR)
	cmake --build "$(BUILDROOT)/RelWithDebugInfo" --verbose
