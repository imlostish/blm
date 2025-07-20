# Makefile for building a COBOL project with modular and single binary options

# Version & Plataform & Project Information
VERSION := 0.0.1-beta
PLATFORM := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ARCH := $(shell uname -m)
BUILD_TYPE := s
ATTEMPT := $(shell if [ -f .buildattempt ]; then echo $$(($$(cat .buildattempt) + 1); else echo 1; fi))

# Build Configuration

BUILD_DIR          := build
BUILD_PROFILE_DIR  := $(BUILD_DIR)/profile
BUILD_DEBUG_DIR    := $(BUILD_DIR)/debug
BUILD_RELEASE_DIR  := $(BUILD_DIR)/release
BUILD_C_DIR        := $(BUILD_DIR)/c
BUILD_ASM_DIR      := $(BUILD_DIR)/asm
BUILD_SBIN_DIR     := $(BUILD_DIR)/sbin
BUILD_BIN_DIR      := $(BUILD_DIR)/bin

PROFILE_REPORT_DIR := $(BUILD_PROFILE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-profile-$(ATTEMPT)
DEBUG_REPORT_DIR   := $(BUILD_DEBUG_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-debug
RELEASE_DIR 	   := $(BUILD_RELEASE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-build

DEBUG_SINGLE_BIN 	:= $(DEBUG_REPORT_DIR)/blm-debug
RELEASE_SINGLE_BIN 	:= $(RELEASE_DIR)/blm
RELEASE_MODULAR_BIN := $(RELEASE_DIR)/blm-modular
PROFILE_BIN 	   	:= $(PROFILE_REPORT_DIR)/blm-profile

MODULE_TYPES := controllers auth utils
MODULE_DIRS := $(foreach type,$(MODULE_TYPES),$(BUILD_BIN_DIR)/$(type) $(BUILD_ASM_DIR)/$(type) $(BUILD_C_DIR)/$(type))

# Update Attempt File
# This file is used to track the number of build attempts
# It is incremented each time the Makefile is run
# and is used to generate unique build directories and filenames
# It is also used to ensure that the build directories are unique
# and do not conflict with previous builds.
.update-attempt:
	@echo $(ATTEMPT) > .buildattempt

# Cobol Compiler Configuration

COBC := cobc
COBC_DEBUG_FLAGS := \
  -g \
  -fsource-location \
  -fstack-extended \
  -fmemory-check=pointer \
  -fimplicit-goback-check \
  -fsection-exit-check \
  -fno-remove-unreachable \
  -O0
COBC_RELEASE_FLAGS := -O2
COBC_COMMON_FLAGS  := -Wothers -Wmissing-newline -free -Wall -I src/core/copybook

# Source Directories and Files

SRC_DIR          := src/core
MAIN_SRC         := $(SRC_DIR)/main.cbl
CLI_SRC          := $(SRC_DIR)/cli.cbl

# Get Source Files for Each Module Type
define get_sources
$(wildcard $(SRC_DIR)/$(1)/*.cbl)
endef

CONTROLLERS_SRC  := $(call get_sources,controllers)
AUTH_SRC         := $(call get_sources,auth)
UTILS_SRC        := $(call get_sources,utils)

# Object Files and Binaries

OBJ_FILES := \
    $(BUILD_BIN_DIR)/main.o \
    $(BUILD_BIN_DIR)/cli.o \
    $(patsubst $(SRC_CONTROLLERS)/%.cbl, $(BUILD_BIN_CONTROLLERS_DIR)/%.o, $(CONTROLLERS_SRC)) \
    $(patsubst $(SRC_AUTH)/%.cbl, $(BUILD_BIN_AUTH_DIR)/%.o, $(AUTH_SRC)) \
    $(patsubst $(SRC_UTILS)/%.cbl, $(BUILD_BIN_UTILS_DIR)/%.o, $(UTILS_SRC))

SINGLE_BINARY    := $(BUILD_SBIN_DIR)/blm_single
MODULAR_BINARY   := $(BUILD_BIN_DIR)/blm

.PHONY: all debug release prepare clean test profile

all: prepare c-generation asm-generation release

prepare:
	@echo "🔧 Preparing build directories..."
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(BUILD_PROFILE_DIR)
	@mkdir -p $(BUILD_DEBUG_DIR)
	@mkdir -p $(BUILD_RELEASE_DIR)
	@mkdir -p $(BUILD_SBIN_DIR)
	@mkdir -p $(DIRS)
	@echo "✅ Build directories prepared"

# -------------------------------------------------------------
# Build Configs
# -------------------------------------------------------------

# Debug build
debug: COBC_FLAGS := $(COBC_COMMON_FLAGS) $(COBC_DEBUG_FLAGS)
debug: prepare debug-single debug-modular asm-generation c-generation

# Release build
release: COBC_FLAGS := $(COBC_COMMON_FLAGS) $(COBC_RELEASE_FLAGS)
release: prepare release-single release-modular

# -------------------------------------------------------------
# Specific Build Targets
# -------------------------------------------------------------

# Single binary - Debug
debug-single: $(BUILD_DEBUG_DIR)/blm_single
$(BUILD_DEBUG_DIR)/blm_single: $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@echo "✅ Debug single binary: $@"

# Modular - Debug
debug-modular: $(BUILD_DEBUG_DIR)/blm_modular
$(BUILD_DEBUG_DIR)/blm_modular: $(patsubst $(SRC_DIR)/%,$(BUILD_DEBUG_DIR)/%.o,$(filter-out $(MAIN_SRC) $(CLI_SRC),$(wildcard $(SRC_DIR)/*.cbl)))
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@echo "✅ Debug modular binary: $@"

# Single binary - Release
release-single: $(BUILD_RELEASE_DIR)/blm_single
$(BUILD_RELEASE_DIR)/blm_single: $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@echo "✅ Release single binary: $@"

# Modular - Release
release-modular: $(BUILD_RELEASE_DIR)/mbinary
$(BUILD_RELEASE)/mbinary: $(patsubst $(SRC_DIR)/%,$(BUILD_RELEASE_DIR)/%.o,$(filter-out $(MAIN_SRC) $(CLI_SRC),$(wildcard $(SRC_DIR)/*.cbl)))
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@echo "✅ Release modular binary: $@"

# -------------------------------------------------------------
# Mid Code Generation
# -------------------------------------------------------------

# Assembly Generation
asm-generation: $(patsubst $(SRC_DIR)/%.cbl,$(BUILD_ASM_DIR)/%.asm,$(wildcard $(SRC_DIR)/*/*.cbl))
$(BUILD_ASM_DIR)/%.asm: $(SRC_DIR)/%.cbl
	$(COBC) $(COBC_FLAGS) -S $< -o $@

# C Generation
c-generation: $(patsubst $(SRC_DIR)/%.cbl,$(BUILD_C_DIR)/%.c,$(wildcard $(SRC_DIR)/*/*.cbl))
$(BUILD_C_DIR)/%.c: $(SRC_DIR)/%.cbl
	$(COBC) $(COBC_FLAGS) -C $< -o $@


# -------------------------------------------------------------
# Utilitys and Testing
# -------------------------------------------------------------

# Testing
test: debug
	@echo "🧪 Running tests..."
	@$(BUILD_DEBUG_DIR)/blm_single --test
	@echo "✅ Tests completed"

# Profiling
profile: COBC_FLAGS := $(COBC_COMMON_FLAGS) -pg
profile: prepare $(BUILD_DIR)/profile/blm_profile
	@echo "📊 Profiling build ready"

$(BUILD_DIR)/profile/blm_profile: $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@

# Clean Build
clean:
	rm -rf $(BUILD_DIR)
	@echo "🧹 Build cleaned"
