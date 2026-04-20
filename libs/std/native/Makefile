CC ?= gcc
AR ?= ar
RANLIB ?= ranlib
STRIP ?= strip
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

CFLAGS ?= -O3 -DNDEBUG -Wall -Wextra -std=c11 -ffunction-sections -fdata-sections -fno-ident -fno-unwind-tables -fno-asynchronous-unwind-tables
CFLAGS += -I$(SRC_ROOT)/include
LDFLAGS ?=

PREFIX ?= /usr/local
LIBDIR ?= $(PREFIX)/lib
INSTALL ?= install
INSTALL_DATA ?= $(INSTALL) -m 0644
INSTALL_PROGRAM ?= $(INSTALL) -m 0755

UNAME_S := $(shell uname -s 2>/dev/null || echo unknown)
ifeq ($(OS),Windows_NT)
	PLATFORM := windows
else ifneq (,$(findstring MINGW,$(UNAME_S)))
	PLATFORM := windows
else ifneq (,$(findstring MSYS,$(UNAME_S)))
	PLATFORM := windows
else ifneq (,$(findstring CYGWIN,$(UNAME_S)))
	PLATFORM := windows
else ifeq ($(UNAME_S),Darwin)
	PLATFORM := macos
else
	PLATFORM := linux
endif

ifeq ($(PLATFORM),windows)
	SHARED_EXT := dll
	SHARED_FLAGS := -shared
	SHARED_SIZE_FLAGS := -Wl,--gc-sections -Wl,-s
	SHARED_EXPORT_FLAGS := -Wl,--export-all-symbols
	BASE_IMPLIB := $(LIB_DIR)/$(TARGET).dll.a
	BASE_IMPLIB_FLAGS := -Wl,--out-implib,$(BASE_IMPLIB)
	STD_IMPLIB := $(LIB_DIR)/$(STD_TARGET).dll.a
	STD_IMPLIB_FLAGS := -Wl,--out-implib,$(STD_IMPLIB)
	STD_SHARED_BASE_FLAGS :=
	PIC_FLAGS :=
else ifeq ($(PLATFORM),macos)
	SHARED_EXT := dylib
	SHARED_FLAGS := -dynamiclib
	SHARED_SIZE_FLAGS := -Wl,-dead_strip -Wl,-x
	SHARED_EXPORT_FLAGS :=
	BASE_IMPLIB :=
	BASE_IMPLIB_FLAGS :=
	STD_IMPLIB :=
	STD_IMPLIB_FLAGS :=
	STD_SHARED_BASE_FLAGS := -Wl,-rpath,@loader_path
	PIC_FLAGS := -fPIC
else
	SHARED_EXT := so
	SHARED_FLAGS := -shared
	SHARED_SIZE_FLAGS := -Wl,--gc-sections -Wl,-s
	SHARED_EXPORT_FLAGS :=
	BASE_IMPLIB :=
	BASE_IMPLIB_FLAGS :=
	STD_IMPLIB :=
	STD_IMPLIB_FLAGS :=
	STD_SHARED_BASE_FLAGS := -Wl,-rpath,'$$ORIGIN'
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
	@echo STRIP=$(STRIP)
	@echo BUILD_DIR=$(BUILD_DIR)
	@echo BUILD_DIR_NORM=$(BUILD_DIR_NORM)
	@echo CFLAGS=$(CFLAGS)
	@echo LDFLAGS=$(LDFLAGS)
	@echo SHARED_SIZE_FLAGS=$(SHARED_SIZE_FLAGS)
	@echo SHARED_EXPORT_FLAGS=$(SHARED_EXPORT_FLAGS)
	@echo SRC_ROOT=$(SRC_ROOT)
	@echo OBJ_DIR=$(OBJ_DIR)
	@echo STD_OBJ_DIR=$(STD_OBJ_DIR)
	@echo LIB_DIR=$(LIB_DIR)
	@echo SHARED_LIB=$(SHARED_LIB)
	@echo STATIC_LIB=$(STATIC_LIB)
	@echo STD_SHARED_LIB=$(STD_SHARED_LIB)
	@echo STD_STATIC_LIB=$(STD_STATIC_LIB)
	@echo BASE_IMPLIB=$(BASE_IMPLIB)
	@echo STD_IMPLIB=$(STD_IMPLIB)
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
	$(CC) $(SHARED_FLAGS) $(SHARED_SIZE_FLAGS) $(SHARED_EXPORT_FLAGS) $(BASE_IMPLIB_FLAGS) $(LDFLAGS) $^ -o "$@"
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" --strip-unneeded "$@" 2>/dev/null || "$(STRIP)" -s "$@" 2>/dev/null || true ) || true

$(STATIC_LIB): $(OBJ)
	@mkdir -p "$(LIB_DIR)"
	$(AR) rcs "$@" $^
	$(RANLIB) "$@"
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" -g "$@" 2>/dev/null || true ) || true

$(STD_OBJ_STAMP): FORCE $(SHARED_LIB) $(STATIC_LIB)
	@set -e; \
	xlang_exe=""; \
	if [ -f "$(XLANG_EXE)" ]; then \
		xlang_exe="$(XLANG_EXE)"; \
	elif [ -f "$(XLANG_EXE_FALLBACK)" ]; then \
		xlang_exe="$(XLANG_EXE_FALLBACK)"; \
	elif command -v xlang >/dev/null 2>&1; then \
		xlang_exe="xlang"; \
	else \
		echo "xlang executable not found: $(XLANG_EXE)"; \
		echo "xlang executable fallback not found: $(XLANG_EXE_FALLBACK)"; \
		echo "xlang command not found in PATH"; \
		echo "build xlang first (repo root): make compile, or add xlang to PATH"; \
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
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" -g "$@" 2>/dev/null || true ) || true

$(STD_SHARED_LIB): $(STD_OBJ_STAMP) $(SHARED_LIB)
	@mkdir -p "$(LIB_DIR)"
	@objs="$$(find "$(STD_OBJ_DIR_ABS)" -type f -name '*.o' | sort)"; \
	if [ -z "$$objs" ]; then \
		echo "no xlang object files found under $(STD_OBJ_DIR_ABS)"; \
		exit 1; \
	fi; \
	base_link="$(SHARED_LIB)"; \
	if [ -n "$(BASE_IMPLIB)" ] && [ -f "$(BASE_IMPLIB)" ]; then \
		base_link="$(BASE_IMPLIB)"; \
	fi; \
	$(CC) $(SHARED_FLAGS) $(SHARED_SIZE_FLAGS) $(SHARED_EXPORT_FLAGS) $(STD_IMPLIB_FLAGS) $(LDFLAGS) $(STD_SHARED_BASE_FLAGS) $$objs "$$base_link" -o "$@"
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" --strip-unneeded "$@" 2>/dev/null || "$(STRIP)" -s "$@" 2>/dev/null || true ) || true

std_shared: $(STD_SHARED_LIB)

std_static: $(STD_STATIC_LIB)

install: all
	@mkdir -p "$(DESTDIR)$(LIBDIR)"
	$(INSTALL_DATA) "$(STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	$(INSTALL_DATA) "$(STD_STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	@if [ -f "$(BASE_IMPLIB)" ]; then \
		$(INSTALL_DATA) "$(BASE_IMPLIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi
	@if [ -f "$(STD_IMPLIB)" ]; then \
		$(INSTALL_DATA) "$(STD_IMPLIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi
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
	@if [ -n "$(BASE_IMPLIB)" ]; then \
		$(RM) -f "$(DESTDIR)$(LIBDIR)/$(notdir $(BASE_IMPLIB))"; \
	fi
	@if [ -n "$(STD_IMPLIB)" ]; then \
		$(RM) -f "$(DESTDIR)$(LIBDIR)/$(notdir $(STD_IMPLIB))"; \
	fi

clean:
	@if [ "$(BUILD_DIR_NORM)" = "." ] || [ "$(BUILD_DIR_NORM)" = "./" ]; then \
		$(RM) -r "$(OBJ_DIR)" "$(LIB_DIR)"; \
	else \
		$(RM) -r "$(BUILD_DIR_NORM)"; \
	fi
