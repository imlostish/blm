# Makefile for building a COBOL project with modular and single binary options

# Version & Plataform & Project Information
VERSION := 0.0.1-beta
PLATFORM := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ARCH := $(shell uname -m)
BUILD_TYPE := s
ATTEMPT := $(shell if [ -f .buildattempt ]; then echo $$(( $$(cat .buildattempt) + 1 )); else echo 1; fi)

# Build Configuration

COPYBOOK_DIR := $(abspath src/core/copybook)
export COB_COPY_DIR := $(COPYBOOK_DIR):$(COB_COPY_DIR)

BUILD_DIR          := build
BUILD_PROFILE_DIR  := $(BUILD_DIR)/profile
BUILD_DEBUG_DIR    := $(BUILD_DIR)/debug
BUILD_RELEASE_DIR  := $(BUILD_DIR)/release
BUILD_C_DIR        := $(BUILD_DIR)/c
BUILD_ASM_DIR      := $(BUILD_DIR)/asm
BUILD_SBIN_DIR     := $(BUILD_DIR)/sbin
BUILD_BIN_DIR      := $(BUILD_DIR)/bin

PROFILE_REPORT_DIR := $(BUILD_PROFILE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-profile-$(ATTEMPT)/report
DEBUG_REPORT_DIR   := $(BUILD_DEBUG_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-debug
RELEASE_DIR 	   := $(BUILD_RELEASE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-build

RELEASE_SINGLE_BIN 	:= $(RELEASE_DIR)/blm
RELEASE_MODULAR_BIN := $(RELEASE_DIR)/blm-modular

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
  -fno-omit-frame-pointer \
  -fno-optimize-sibling-calls \
  -fno-inline \
  -fno-inline-functions \
  -fno-inline-small-functions \
  -fno-inline-functions-called-once \
  -fno-inline-atomics \
  -O0
COBC_BASE_FLAGS := -Wno-missing-newline -Wno-dialect -free -Wall -I$(COPYBOOK_DIR)
COBC_RELEASE_FLAGS := $(COBC_BASE_FLAGS) -O2
COBC_FULL_DEBUG_FLAGS := $(COBC_BASE_FLAGS) $(COBC_DEBUG_FLAGS) -O0

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


.PHONY: all debug release prepare clean test profile

all: prepare c-generation asm-generation release

prepare:
	@echo "🔧 Preparing build directories..."
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(BUILD_PROFILE_DIR)
	@mkdir -p $(BUILD_DEBUG_DIR)
	@mkdir -p $(BUILD_RELEASE_DIR)
	@mkdir -p $(BUILD_SBIN_DIR)
	@mkdir -p $(MODULE_DIRS)
	@echo "✅ Build directories prepared"

# -------------------------------------------------------------
# Build Configs
# -------------------------------------------------------------

# Debug build
debug: COBC_FLAGS := $(COBC_FULL_DEBUG_FLAGS)
debug: prepare debug-single asm-generation c-generation

# Release build
release: COBC_FLAGS := $(COBC_RELEASE_FLAGS)
release: prepare release-single release-modular

# -------------------------------------------------------------
# Specific Build Targets
# -------------------------------------------------------------

# Single binary - Debug
debug-single: .update-attempt $(DEBUG_REPORT_DIR)
$(DEBUG_REPORT_DIR): $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_DEBUG_FLAGS) -x $^ -o $@
	@mkdir -p $(BUILD_DEBUG_DIR)/debug-info
	@echo "Debug symbols included" > $(BUILD_DEBUG_DIR)/debug-info/debug-features.txt
	@echo "Source locations: enabled" >> $(BUILD_DEBUG_DIR)/debug-info/debug-features.txt
	@echo "Stack tracing: enabled" >> $(BUILD_DEBUG_DIR)/debug-info/debug-features.txt

# Modular - Debug
# debug-modular: $(BUILD_DEBUG_DIR)/blm-modular-debug
# $(BUILD_DEBUG_DIR)/blm-modular-debug: $(patsubst $(SRC_DIR)/%,$(BUILD_DEBUG_DIR)/%.o,$(filter-out $(MAIN_SRC) $(CLI_SRC),$(wildcard $(SRC_DIR)/*.cbl)))
# 	$(COBC) $(COBC_FLAGS) -x $^ -o $@
# 	@echo "✅ Debug modular binary: $@"

# Single binary - Release
release-single: BUILD_TYPE := s
release-single: .update-attempt $(RELEASE_SINGLE_BIN)
$(RELEASE_SINGLE_BIN): $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	@mkdir -p $(@D)
	$(COBC) $(COBC_RELEASE_FLAGS) -x $^ -o $@
	@mkdir -p $(BUILD_RELEASE_DIR)/reports
	@echo "Build completed: $$(date)" > $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Version: $(VERSION)" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Platform: $(PLATFORM)-$(ARCH)" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Type: single" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt

# Modular - Release
release-modular: BUILD_TYPE := m
release-modular: .update-attempt $(RELEASE_MODULAR_BIN)
$(RELEASE_MODULAR_BIN): $(patsubst $(SRC_DIR)/%,$(BUILD_RELEASE_DIR)/modules/%.o,$(filter-out $(MAIN_SRC) $(CLI_SRC),$(wildcard $(SRC_DIR)/*.cbl)))
	$(COBC) $(COBC_RELEASE_FLAGS) -x $^ -o $@
	@mkdir -p $(BUILD_RELEASE_DIR)/reports
	@echo "Build completed: $$(date)" > $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Version: $(VERSION)" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Platform: $(PLATFORM)-$(ARCH)" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Type: modular" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@echo "Modules:" >> $(BUILD_RELEASE_DIR)/reports/build-info.txt
	@ls $(BUILD_RELEASE_DIR)/modules >> $(BUILD_RELEASE_DIR)/reports/build-info.txt

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
	@mkdir -p $(@D)
	@echo "Generating C code from $<"
	$(COBC) $(COBC_FLAGS) -v -C $< -o $@
	@echo "Generated $@"


# -------------------------------------------------------------
# Utilitys and Testing
# -------------------------------------------------------------

# Testing
test: debug
	@echo "🧪 Running tests..."
	@$(BUILD_DEBUG_DIR)/blm_single --test
	@echo "✅ Tests completed"

# Profiling
profile: BUILD_TYPE := s
profile: COBC_FLAGS := $(COBC_FULL_DEBUG_FLAGS) -pg
profile: .update-attempt $(PROFILE_REPORT_DIR)
$(PROFILE_REPORT_DIR): $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@mkdir -p $(PROFILE_REPORT_DIR)
	@echo "Profiling build completed: $$(date)" > $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Version: $(VERSION)" >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Platform: $(PLATFORM)-$(ARCH)" >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Type: profile" >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Modules:" >> $(PROFILE_REPORT_DIR)/build-info.txt
	@ls $(BUILD_PROFILE_DIR) >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Profiling data generated in $(PROFILE_REPORT_DIR)"
	@echo "Run 'gprof $(PROFILE_REPORT_DIR) gmon.out' to analyze profiling data"
	@echo "Run 'gprof $(PROFILE_REPORT_DIR) gmon.out > profile.txt' to save profiling report"
	@echo "Run 'gprof $(PROFILE_REPORT_DIR) gmon.out | less' to view profiling report"
	@echo "Run 'gprof $(PROFILE_REPORT_DIR) gmon.out | grep -i 'function_name' to filter profiling report"
	@echo "📊 Profiling build ready"

$(BUILD_DIR)/profile/blm_profile: $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC) $(UTILS_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@

# Clean Build
clean:
	rm -rf $(BUILD_DIR)
	@echo "🧹 Build cleaned"
