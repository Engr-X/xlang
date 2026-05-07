CC ?= gcc
AR ?= ar
RANLIB ?= ranlib
STRIP ?= strip
DLLTOOL ?= dlltool
OBJDUMP ?= objdump

MAKEFILE_PATH := $(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR := $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))

CONFIG_MK ?= config.mk
-include $(CONFIG_MK)

SRC_ROOT ?= $(MAKEFILE_DIR)

TARGET ?= libxlang-core
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

CORE_C_SOURCES := \
	xlang/runtime/io/console.c \
	xlang/runtime/memory/sysmem.c \
	xlang/runtime/math.c \
	xlang/runtime/system.c

OBJ := $(addprefix $(OBJ_DIR)/,$(CORE_C_SOURCES:.c=.o))

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
	SHARED_VERSION_FLAGS :=
	SHARED_EXPORT_FLAGS := -Wl,--export-all-symbols
	IMPLIB := $(LIB_DIR)/$(TARGET).dll.a
	IMPLIB_FLAGS := -Wl,--out-implib,$(IMPLIB)
	DEF_FILE := $(LIB_DIR)/$(TARGET).def
	PIC_FLAGS :=
	CFLAGS += -D__USE_MINGW_ANSI_STDIO=0
else ifeq ($(PLATFORM),macos)
	SHARED_EXT := dylib
	SHARED_FLAGS := -dynamiclib
	SHARED_SIZE_FLAGS := -Wl,-dead_strip -Wl,-x
	SHARED_VERSION_FLAGS :=
	SHARED_EXPORT_FLAGS :=
	IMPLIB :=
	IMPLIB_FLAGS :=
	DEF_FILE :=
	PIC_FLAGS := -fPIC
else
	SHARED_EXT := so
	SHARED_FLAGS := -shared
	SHARED_SIZE_FLAGS := -Wl,--gc-sections -Wl,-s
	SHARED_VERSION_FLAGS := -Wl,--undefined-version
	SHARED_EXPORT_FLAGS :=
	IMPLIB :=
	IMPLIB_FLAGS :=
	DEF_FILE :=
	PIC_FLAGS := -fPIC
endif

ifneq ($(PLATFORM),windows)
	LDFLAGS += -lm
endif

ifeq ($(PLATFORM),linux)
	CFLAGS += -U__USE_MINGW_ANSI_STDIO
endif

SHARED_LIB := $(LIB_DIR)/$(TARGET).$(SHARED_EXT)
STATIC_LIB := $(LIB_DIR)/$(TARGET).a

.PHONY: all shared static clean info install uninstall

all: shared static

info:
	@echo PLATFORM=$(PLATFORM)
	@echo CC=$(CC)
	@echo TARGET=$(TARGET)
	@echo STRIP=$(STRIP)
	@echo BUILD_DIR=$(BUILD_DIR)
	@echo BUILD_DIR_NORM=$(BUILD_DIR_NORM)
	@echo CFLAGS=$(CFLAGS)
	@echo LDFLAGS=$(LDFLAGS)
	@echo SHARED_SIZE_FLAGS=$(SHARED_SIZE_FLAGS)
	@echo SHARED_EXPORT_FLAGS=$(SHARED_EXPORT_FLAGS)
	@echo SRC_ROOT=$(SRC_ROOT)
	@echo OBJ_DIR=$(OBJ_DIR)
	@echo LIB_DIR=$(LIB_DIR)
	@echo SHARED_LIB=$(SHARED_LIB)
	@echo STATIC_LIB=$(STATIC_LIB)
	@echo IMPLIB=$(IMPLIB)

$(OBJ_DIR)/%.o: $(SRC_ROOT)/src/%.c
	@mkdir -p "$(@D)"
	$(CC) $(CFLAGS) $(PIC_FLAGS) -c "$<" -o "$@"

$(SHARED_LIB): $(OBJ)
	@mkdir -p "$(LIB_DIR)"
	$(CC) $(SHARED_FLAGS) $(SHARED_SIZE_FLAGS) $(SHARED_VERSION_FLAGS) $(SHARED_EXPORT_FLAGS) $(IMPLIB_FLAGS) $(LDFLAGS) $^ -o "$@"
ifeq ($(PLATFORM),windows)
	@if command -v "$(DLLTOOL)" >/dev/null 2>&1; then \
		syms=$$( $(OBJDUMP) -p "$@" | grep 'base\[' | grep -v 'Export RVA' | awk '{print $$NF}' | sort -u ); \
		if [ -n "$$syms" ]; then \
			{ \
				echo "LIBRARY $(notdir $(SHARED_LIB))"; \
				echo "EXPORTS"; \
				printf '%s\n' "$$syms"; \
			} > "$(DEF_FILE)"; \
			"$(DLLTOOL)" -d "$(DEF_FILE)" -D "$(notdir $(SHARED_LIB))" -l "$(IMPLIB)"; \
		else \
			echo "[WARN] no exports found in $(SHARED_LIB), keep linker-generated import lib: $(IMPLIB)"; \
		fi; \
	else \
		echo "[WARN] dlltool not found, keep linker-generated import lib: $(IMPLIB)"; \
	fi
endif
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" --strip-unneeded "$@" 2>/dev/null || "$(STRIP)" -s "$@" 2>/dev/null || true ) || true

$(STATIC_LIB): $(OBJ)
	@mkdir -p "$(LIB_DIR)"
	$(AR) rcs "$@" $^
	$(RANLIB) "$@"
	@command -v "$(STRIP)" >/dev/null 2>&1 && ( "$(STRIP)" -g "$@" 2>/dev/null || true ) || true

shared: $(SHARED_LIB)

static: $(STATIC_LIB)

install: all
	@mkdir -p "$(DESTDIR)$(LIBDIR)"
	$(INSTALL_DATA) "$(STATIC_LIB)" "$(DESTDIR)$(LIBDIR)/"
	@if [ -f "$(IMPLIB)" ]; then \
		$(INSTALL_DATA) "$(IMPLIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi
	@if [ -f "$(SHARED_LIB)" ]; then \
		$(INSTALL_PROGRAM) "$(SHARED_LIB)" "$(DESTDIR)$(LIBDIR)/"; \
	fi

uninstall:
	$(RM) -f \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(STATIC_LIB))" \
		"$(DESTDIR)$(LIBDIR)/$(notdir $(SHARED_LIB))"
	@if [ -n "$(IMPLIB)" ]; then \
		$(RM) -f "$(DESTDIR)$(LIBDIR)/$(notdir $(IMPLIB))"; \
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
