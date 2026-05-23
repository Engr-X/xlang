CC ?= gcc
AR ?= ar
RANLIB ?= ranlib
STRIP ?= strip
DLLTOOL ?= dlltool
OBJDUMP ?= objdump
XLANG_JOBS ?= 1

MAKEFILE_PATH := $(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR := $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))
REPO_ROOT := $(abspath $(MAKEFILE_DIR)/../../..)

CONFIG_MK ?= config.mk
-include $(CONFIG_MK)

SRC_ROOT ?= $(MAKEFILE_DIR)

TARGET ?= libxlang-std
BASE_TARGET ?= libxlang-core
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

XLANG_SOURCES := \
	xlang/Math.x \
	xlang/System.x \
	xlang/functional/List.x \
	xlang/sort/HeapSort.x \
	xlang/io/Console.x

STD_OBJ_DIR := $(OBJ_DIR)/xlang-std
STD_OBJ_STAMP := $(STD_OBJ_DIR)/.built
STD_OBJ_DIR_ABS := $(abspath $(STD_OBJ_DIR))
STD_OBJ_STAMP_ABS := $(STD_OBJ_DIR_ABS)/.built

CFLAGS ?= -O3 -DNDEBUG -Wall -Wextra -std=c11 -ffunction-sections -fdata-sections -fno-ident -fno-unwind-tables -fno-asynchronous-unwind-tables
LDFLAGS ?=

PREFIX ?= /usr/local
LIBDIR ?= $(PREFIX)/lib
INSTALL ?= install
INSTALL_DATA ?= $(INSTALL) -m 0644
INSTALL_PROGRAM ?= $(INSTALL) -m 0755

RUNTIME_CORE_DIR ?= $(REPO_ROOT)/runtime
RUNTIME_CORE_BUILD_DIR ?= $(RUNTIME_CORE_DIR)/build
RUNTIME_CORE_LIB_DIR ?= $(RUNTIME_CORE_BUILD_DIR)/libs

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
	SHARED_VERSION_FLAGS :=
	SHARED_EXPORT_FLAGS := -Wl,--export-all-symbols
	TARGET_IMPLIB := $(LIB_DIR)/$(TARGET).dll.a
	TARGET_IMPLIB_FLAGS := -Wl,--out-implib,$(TARGET_IMPLIB)
	TARGET_DEF := $(LIB_DIR)/$(TARGET).def
	CORE_LINK_CANDIDATES := \
		$(RUNTIME_CORE_LIB_DIR)/$(BASE_TARGET).dll.a \
		$(RUNTIME_CORE_LIB_DIR)/$(BASE_TARGET).a
else ifeq ($(PLATFORM),macos)
	SHARED_EXT := dylib
	SHARED_FLAGS := -dynamiclib
	SHARED_SIZE_FLAGS := -Wl,-dead_strip -Wl,-x
	SHARED_VERSION_FLAGS :=
	SHARED_EXPORT_FLAGS :=
	TARGET_IMPLIB :=
	TARGET_IMPLIB_FLAGS :=
	TARGET_DEF :=
	CORE_LINK_CANDIDATES := $(RUNTIME_CORE_LIB_DIR)/$(BASE_TARGET).a
else
	SHARED_EXT := so
	SHARED_FLAGS := -shared
	SHARED_SIZE_FLAGS := -Wl,--gc-sections -Wl,-s
	SHARED_VERSION_FLAGS := -Wl,--undefined-version
	SHARED_EXPORT_FLAGS :=
	TARGET_IMPLIB :=
	TARGET_IMPLIB_FLAGS :=
	TARGET_DEF :=
	CORE_LINK_CANDIDATES := $(RUNTIME_CORE_LIB_DIR)/$(BASE_TARGET).a
endif

ifneq ($(PLATFORM),windows)
	LDFLAGS += -lm
endif

STD_SHARED_LIB := $(LIB_DIR)/$(TARGET).$(SHARED_EXT)
STD_STATIC_LIB := $(LIB_DIR)/$(TARGET).a

XLANG_SRC_ROOT := $(SRC_ROOT)/src/xlang
ifeq ($(PLATFORM),windows)
XLANG_EXE ?= $(REPO_ROOT)/build/xlang.exe
XLANG_EXE_FALLBACK ?= $(REPO_ROOT)/build/xlang
XLANG_X64_COMPILER_FLAG :=
else
XLANG_EXE ?= $(REPO_ROOT)/build/xlang
XLANG_EXE_FALLBACK ?= $(REPO_ROOT)/build/xlang.exe
XLANG_X64_COMPILER_FLAG := --compiler=as
endif

.PHONY: all std std_shared std_static clean info install uninstall FORCE

all: std

std: std_shared std_static

info:
	@echo PLATFORM=$(PLATFORM)
	@echo TARGET=$(TARGET)
	@echo BASE_TARGET=$(BASE_TARGET)
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
	@echo STD_SHARED_LIB=$(STD_SHARED_LIB)
	@echo STD_STATIC_LIB=$(STD_STATIC_LIB)
	@echo TARGET_IMPLIB=$(TARGET_IMPLIB)
	@echo XLANG_EXE=$(XLANG_EXE)
	@echo XLANG_EXE_FALLBACK=$(XLANG_EXE_FALLBACK)
	@echo REPO_ROOT=$(REPO_ROOT)
	@echo XLANG_SRC_ROOT=$(XLANG_SRC_ROOT)
	@echo XLANG_SOURCES=$(XLANG_SOURCES)
	@echo RUNTIME_CORE_LIB_DIR=$(RUNTIME_CORE_LIB_DIR)

$(STD_OBJ_STAMP): FORCE
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
	srcs="$(XLANG_SOURCES)"; \
	if [ -z "$$srcs" ]; then \
		echo "no xlang sources found under $(XLANG_SRC_ROOT)"; \
		exit 1; \
	fi; \
	for src in $$srcs; do \
		if [ ! -f "$(XLANG_SRC_ROOT)/$$src" ]; then \
			echo "xlang source not found: $(XLANG_SRC_ROOT)/$$src"; \
			exit 1; \
		fi; \
	done; \
	"$$xlang_exe" --target=x64 $(XLANG_X64_COMPILER_FLAG) --root="$(XLANG_SRC_ROOT)" $$srcs -d "$(STD_OBJ_DIR_ABS)" -j$(XLANG_JOBS); \
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

$(STD_SHARED_LIB): $(STD_OBJ_STAMP)
	@mkdir -p "$(LIB_DIR)"
	@objs="$$(find "$(STD_OBJ_DIR_ABS)" -type f -name '*.o' | sort)"; \
	if [ -z "$$objs" ]; then \
		echo "no xlang object files found under $(STD_OBJ_DIR_ABS)"; \
		exit 1; \
	fi; \
	base_link=""; \
	for c in $(CORE_LINK_CANDIDATES); do \
		if [ -f "$$c" ]; then \
			base_link="$$c"; \
			break; \
		fi; \
	done; \
	if [ -z "$$base_link" ]; then \
		echo "runtime core library not found under $(RUNTIME_CORE_LIB_DIR)"; \
		echo "expected one of: $(CORE_LINK_CANDIDATES)"; \
		echo "build runtime first"; \
		exit 1; \
	fi; \
	$(CC) $(SHARED_FLAGS) $(SHARED_SIZE_FLAGS) $(SHARED_VERSION_FLAGS) $(SHARED_EXPORT_FLAGS) $(TARGET_IMPLIB_FLAGS) $(LDFLAGS) $$objs "$$base_link" -o "$@"
ifeq ($(PLATFORM),windows)
	@if command -v "$(DLLTOOL)" >/dev/null 2>&1; then \
		syms=$$( $(OBJDUMP) -p "$@" | grep 'base\[' | grep -v 'Export RVA' | awk '{print $$NF}' | sort -u ); \
		if [ -n "$$syms" ]; then \
			{ \
				echo "LIBRARY $(notdir $(STD_SHARED_LIB))"; \
				echo "EXPORTS"; \
				printf '%s\n' "$$syms"; \
			} > "$(TARGET_DEF)"; \
			"$(DLLTOOL)" -d "$(TARGET_DEF)" -D "$(notdir $(STD_SHARED_LIB))" -l "$(TARGET_IMPLIB)"; \
		else \
			echo "[WARN] no exports found in $(STD_SHARED_LIB), keep linker-generated import lib: $(TARGET_IMPLIB)"; \
		fi; \
	else \
		echo "[WARN] dlltool not found, keep linker-generated import lib: $(TARGET_IMPLIB)"; \
	fi
endif
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" --strip-unneeded "$@" 2>/dev/null || "$(STRIP)" -s "$@" 2>/dev/null || true ) || true

std_shared: $(STD_SHARED_LIB)

std_static: $(STD_STATIC_LIB)

install: all
	@mkdir -p "$(DESTDIR)$(LIBDIR)"
	$(INSTALL_DATA) "$(STD_STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	@if [ -f "$(TARGET_IMPLIB)" ]; then \
		$(INSTALL_DATA) "$(TARGET_IMPLIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi
	@if [ -f "$(STD_SHARED_LIB)" ]; then \
		$(INSTALL_PROGRAM) "$(STD_SHARED_LIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi

uninstall:
	$(RM) -f \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STD_STATIC_LIB))" \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STD_SHARED_LIB))"
	@if [ -n "$(TARGET_IMPLIB)" ]; then \
		$(RM) -f "$(DESTDIR)$(LIBDIR)/$(notdir $(TARGET_IMPLIB))"; \
	fi

clean:
	@if [ "$(BUILD_DIR_NORM)" = "." ] || [ "$(BUILD_DIR_NORM)" = "./" ]; then \
		$(RM) -r "$(OBJ_DIR)" 2>/dev/null || true; \
		if ! $(RM) -r "$(LIB_DIR)" 2>/dev/null; then \
			if [ -d "$(LIB_DIR)" ]; then \
				echo "[WARN] clean: '$(LIB_DIR)' is busy, fallback to deleting contents only"; \
				find "$(LIB_DIR)" -mindepth 1 -type f -delete 2>/dev/null || true; \
				find "$(LIB_DIR)" -mindepth 1 -type d -empty -delete 2>/dev/null || true; \
			fi; \
		fi; \
	else \
		$(RM) -r "$(BUILD_DIR_NORM)" 2>/dev/null || true; \
	fi
