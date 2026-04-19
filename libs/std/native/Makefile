CC ?= gcc
AR ?= ar
RANLIB ?= ranlib
XLANG_JOBS ?= 1

MAKEFILE_PATH := $(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR := $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))
REPO_ROOT := $(abspath $(MAKEFILE_DIR)/../../..)

CONFIG_MK ?= config.mk
-include $(CONFIG_MK)

SRC_ROOT ?= $(MAKEFILE_DIR)

TARGET ?= libxlang-base
STD_TARGET ?= libxlang-std
BUILD_DIR ?= build
BUILD_DIR_NORM := $(patsubst ./%,%,$(BUILD_DIR))
BUILD_DIR_NORM := $(patsubst %/,%,$(BUILD_DIR_NORM))
ifeq ($(BUILD_DIR_NORM),.)
OBJ_DIR := obj
LIB_DIR := libs
else
OBJ_DIR := $(BUILD_DIR_NORM)/obj
LIB_DIR := $(BUILD_DIR_NORM)/libs
endif

SRC_CONSOLE := $(SRC_ROOT)/src/c/xlang/io/console.c
SRC_MATH := $(SRC_ROOT)/src/c/xlang/math.c

OBJ_CONSOLE := $(OBJ_DIR)/xlang/io/console.o
OBJ_MATH := $(OBJ_DIR)/xlang/math.o
OBJ := $(OBJ_CONSOLE) $(OBJ_MATH)
STD_OBJ_DIR := $(OBJ_DIR)/xlang-std
STD_OBJ_STAMP := $(STD_OBJ_DIR)/.built
STD_OBJ_DIR_ABS := $(abspath $(STD_OBJ_DIR))
STD_OBJ_STAMP_ABS := $(STD_OBJ_DIR_ABS)/.built

CFLAGS ?= -O3 -Wall -Wextra -std=c11
CFLAGS += -I$(SRC_ROOT)/include

PREFIX ?= /usr/local
LIBDIR ?= $(PREFIX)/lib
INSTALL ?= install
INSTALL_DATA ?= $(INSTALL) -m 0644
INSTALL_PROGRAM ?= $(INSTALL) -m 0755

ifeq ($(OS),Windows_NT)
	PLATFORM := windows
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		PLATFORM := macos
	else
		PLATFORM := linux
	endif
endif

ifeq ($(PLATFORM),windows)
	SHARED_EXT := dll
	SHARED_FLAGS := -shared
	PIC_FLAGS :=
else ifeq ($(PLATFORM),macos)
	SHARED_EXT := dylib
	SHARED_FLAGS := -dynamiclib
	PIC_FLAGS := -fPIC
else
	SHARED_EXT := so
	SHARED_FLAGS := -shared
	PIC_FLAGS := -fPIC
endif

SHARED_LIB := $(LIB_DIR)/$(TARGET).$(SHARED_EXT)
STATIC_LIB := $(LIB_DIR)/$(TARGET).a
STD_SHARED_LIB := $(LIB_DIR)/$(STD_TARGET).$(SHARED_EXT)
STD_STATIC_LIB := $(LIB_DIR)/$(STD_TARGET).a

XLANG_SRC_ROOT := $(SRC_ROOT)/src/xlang
ifeq ($(PLATFORM),windows)
XLANG_EXE ?= $(REPO_ROOT)/build/xlang.exe
XLANG_EXE_FALLBACK ?= $(REPO_ROOT)/build/xlang
else
XLANG_EXE ?= $(REPO_ROOT)/build/xlang
XLANG_EXE_FALLBACK ?= $(REPO_ROOT)/build/xlang.exe
endif

.PHONY: all base std shared static std_shared std_static clean info install uninstall FORCE

all: base std

base: shared static

std: std_shared std_static

info:
	@echo PLATFORM=$(PLATFORM)
	@echo CC=$(CC)
	@echo TARGET=$(TARGET)
	@echo STD_TARGET=$(STD_TARGET)
	@echo BUILD_DIR=$(BUILD_DIR)
	@echo BUILD_DIR_NORM=$(BUILD_DIR_NORM)
	@echo SRC_ROOT=$(SRC_ROOT)
	@echo OBJ_DIR=$(OBJ_DIR)
	@echo STD_OBJ_DIR=$(STD_OBJ_DIR)
	@echo LIB_DIR=$(LIB_DIR)
	@echo SHARED_LIB=$(SHARED_LIB)
	@echo STATIC_LIB=$(STATIC_LIB)
	@echo STD_SHARED_LIB=$(STD_SHARED_LIB)
	@echo STD_STATIC_LIB=$(STD_STATIC_LIB)
	@echo XLANG_EXE=$(XLANG_EXE)
	@echo XLANG_EXE_FALLBACK=$(XLANG_EXE_FALLBACK)
	@echo REPO_ROOT=$(REPO_ROOT)
	@echo XLANG_SRC_ROOT=$(XLANG_SRC_ROOT)

shared: $(SHARED_LIB)

static: $(STATIC_LIB)

$(OBJ_CONSOLE): $(SRC_CONSOLE)
	@mkdir -p "$(@D)"
	$(CC) $(CFLAGS) $(PIC_FLAGS) -c "$<" -o "$@"

$(OBJ_MATH): $(SRC_MATH)
	@mkdir -p "$(@D)"
	$(CC) $(CFLAGS) $(PIC_FLAGS) -c "$<" -o "$@"

$(SHARED_LIB): $(OBJ)
	@mkdir -p "$(LIB_DIR)"
	$(CC) $(SHARED_FLAGS) $^ -o "$@"

$(STATIC_LIB): $(OBJ)
	@mkdir -p "$(LIB_DIR)"
	$(AR) rcs "$@" $^
	$(RANLIB) "$@"

$(STD_OBJ_STAMP): FORCE $(SHARED_LIB) $(STATIC_LIB)
	@set -e; \
	xlang_exe=""; \
	if [ -f "$(XLANG_EXE)" ]; then \
		xlang_exe="$(XLANG_EXE)"; \
	elif [ -f "$(XLANG_EXE_FALLBACK)" ]; then \
		xlang_exe="$(XLANG_EXE_FALLBACK)"; \
	else \
		echo "xlang executable not found: $(XLANG_EXE)"; \
		echo "xlang executable fallback not found: $(XLANG_EXE_FALLBACK)"; \
		echo "build xlang first (repo root): make compile"; \
		exit 1; \
	fi; \
	mkdir -p "$(STD_OBJ_DIR_ABS)"; \
	find "$(STD_OBJ_DIR_ABS)" -type f \( -name '*.o' -o -name '.built' \) -delete; \
	srcs="$$(cd "$(XLANG_SRC_ROOT)" && find . -type f \( -name '*.x' -o -name '*.xl' -o -name '*.xlang' \) | sed 's|^\./||' | sort)"; \
	if [ -z "$$srcs" ]; then \
		echo "no xlang sources found under $(XLANG_SRC_ROOT)"; \
		exit 1; \
	fi; \
	"$$xlang_exe" --target=x64 --root="$(XLANG_SRC_ROOT)" $$srcs -d "$(STD_OBJ_DIR_ABS)" -j$(XLANG_JOBS); \
	objs="$$(find "$(STD_OBJ_DIR_ABS)" -type f -name '*.o' | sort)"; \
	if [ -z "$$objs" ]; then \
		echo "xlang compile finished but produced no .o files under $(STD_OBJ_DIR_ABS)"; \
		exit 1; \
	fi; \
	touch "$(STD_OBJ_STAMP_ABS)"

$(STD_STATIC_LIB): $(STD_OBJ_STAMP)
	@mkdir -p "$(LIB_DIR)"
	@objs="$$(find "$(STD_OBJ_DIR_ABS)" -type f -name '*.o' | sort)"; \
	if [ -z "$$objs" ]; then \
		echo "no xlang object files found under $(STD_OBJ_DIR_ABS)"; \
		exit 1; \
	fi; \
	$(AR) rcs "$@" $$objs; \
	$(RANLIB) "$@"

$(STD_SHARED_LIB): $(STD_OBJ_STAMP)
	@mkdir -p "$(LIB_DIR)"
	@objs="$$(find "$(STD_OBJ_DIR_ABS)" -type f -name '*.o' | sort)"; \
	if [ -z "$$objs" ]; then \
		echo "no xlang object files found under $(STD_OBJ_DIR_ABS)"; \
		exit 1; \
	fi; \
	$(CC) $(SHARED_FLAGS) $$objs "$(STATIC_LIB)" -o "$@"

std_shared: $(STD_SHARED_LIB)

std_static: $(STD_STATIC_LIB)

install: all
	@mkdir -p "$(DESTDIR)$(LIBDIR)"
	$(INSTALL_DATA) "$(STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	$(INSTALL_DATA) "$(STD_STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	@if [ -f "$(SHARED_LIB)" ]; then \
		$(INSTALL_PROGRAM) "$(SHARED_LIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi
	@if [ -f "$(STD_SHARED_LIB)" ]; then \
		$(INSTALL_PROGRAM) "$(STD_SHARED_LIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi

uninstall:
	$(RM) -f \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STATIC_LIB))" \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(SHARED_LIB))" \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STD_STATIC_LIB))" \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STD_SHARED_LIB))"

clean:
	@if [ "$(BUILD_DIR_NORM)" = "." ] || [ "$(BUILD_DIR_NORM)" = "./" ]; then \
		$(RM) -r "$(OBJ_DIR)" "$(LIB_DIR)"; \
	else \
		$(RM) -r "$(BUILD_DIR_NORM)"; \
	fi
