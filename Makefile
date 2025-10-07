# Makefile for building a COBOL project with modular and single binary options

# Version & Plataform & Project Information
VERSION := 0.0.1-beta
PLATFORM := $(shell uname -s | tr '[:upper:]' '[:lower:]')
ARCH := $(shell uname -m)
BUILD_TYPE := s
ATTEMPT := $(shell if [ -f .buildattempt ]; then echo $$(( $$(cat .buildattempt) + 1 )); else echo 1; fi)

# Build Configuration

COPYBOOK_DIR := $(abspath src/core/entities)
export COB_COPY_DIR := $(COPYBOOK_DIR):$(COB_COPY_DIR)

BUILD_DIR          := build
BUILD_PROFILE_DIR  := $(BUILD_DIR)/profile
BUILD_DEBUG_DIR    := $(BUILD_DIR)/debug
BUILD_RELEASE_DIR  := $(BUILD_DIR)/release
BUILD_C_DIR        := $(BUILD_DIR)/c
BUILD_ASM_DIR      := $(BUILD_DIR)/asm

PROFILE_REPORT_DIR := $(BUILD_PROFILE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-profile-$(ATTEMPT)/report
DEBUG_REPORT_DIR   := $(BUILD_DEBUG_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(BUILD_TYPE)-debug
RELEASE_DIR 	   := $(BUILD_RELEASE_DIR)/blm-$(VERSION)-$(PLATFORM)-$(ARCH)-$(ATTEMPT)-build
RELEASE_BIN		   := $(RELEASE_DIR)/blm
# Source Directories and Files

SRC_CORE_DIR          := src/core
SRC_LIB_DIR           := src/lib
SRC_PLUGINS_DIR	      := src/plugins
SRC_TEST_DIR	      := src/test

MAIN_SRC := $(SRC_CORE_DIR)/main.cbl
CLI_SRC  := $(SRC_CORE_DIR)/cli.cbl
ALL_CORE_SRC := \
	$(SRC_CORE_DIR)/controllers/payments-controller.cbl \
	$(SRC_CORE_DIR)/controllers/user-controller.cbl \
	$(SRC_CORE_DIR)/controllers/creditcard-controller.cbl \
	$(SRC_CORE_DIR)/controllers/profile-controller.cbl \
	$(SRC_CORE_DIR)/auth/user-auth.cbl \
	$(SRC_CORE_DIR)/auth/args-auth.cbl \
	$(SRC_CORE_DIR)/utils/email-utils.cbl \
	$(SRC_CORE_DIR)/cli.cbl \
	$(MAIN_SRC)
ASM_TARGETS := $(patsubst $(SRC_CORE_DIR)/%.cbl,$(BUILD_ASM_DIR)/%.asm,$(ALL_CORE_SRC))
C_TARGETS   := $(patsubst $(SRC_CORE_DIR)/%.cbl,$(BUILD_C_DIR)/%.c,$(ALL_CORE_SRC))

# Module Directories
MODULE_TYPES := controllers auth utils
MODULE_DIRS := $(foreach type,$(MODULE_TYPES),$(BUILD_ASM_DIR)/$(type) $(BUILD_C_DIR)/$(type))

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
  -U_FORTIFY_SOURCE

COBC_BASE_FLAGS := \
  -Wno-missing-newline \
  -Wno-dialect \
  -free \
  -Wall \
  -I$(COPYBOOK_DIR)

COBC_RELEASE_FLAGS := $(COBC_BASE_FLAGS) -O2
COBC_FULL_DEBUG_FLAGS := $(COBC_BASE_FLAGS) $(COBC_DEBUG_FLAGS)

# Compilation section

.PHONY: all debug release prepare clean test profile

all: c-generation asm-generation release

prepare:
	@echo "🔧 Preparing build directories..."
	@mkdir -p $(BUILD_DIR) $(BUILD_ASM_DIR) $(BUILD_C_DIR)
	@mkdir -p $(BUILD_DEBUG_DIR) $(BUILD_RELEASE_DIR) $(BUILD_PROFILE_DIR)
	@mkdir -p $(MODULE_DIRS)
	@echo "✅ Build directories prepared"

# -------------------------------------------------------------
# Build Configs
# -------------------------------------------------------------

# Debug build
debug: COBC_FLAGS := $(COBC_FULL_DEBUG_FLAGS)
debug: prepare asm-generation c-generation

# Release build
release: COBC_FLAGS := $(COBC_RELEASE_FLAGS)
release: prepare .update-attempt $(RELEASE_BIN)
$(RELEASE_BIN): $(ALL_CORE_SRC)
	@mkdir -p $(dir $@)
	@echo "🚀 Building single release binary: $@"
	$(COBC) $(COBC_RELEASE_FLAGS) -x $(ALL_CORE_SRC) -o $@
	@echo "✅ Binary created successfully!"

# -------------------------------------------------------------
# Mid Code Generation
# -------------------------------------------------------------

# Assembly Generation
asm-generation: $(ASM_TARGETS)
$(BUILD_ASM_DIR)/%.asm: $(SRC_CORE_DIR)/%.cbl
	@mkdir -p $(@D)
	@echo "🛠️  Generando ASM de $<"
	$(COBC) $(COBC_FLAGS) -S $< -o $@
	@echo "✅ Generado $@"

# C Generation
c-generation: $(C_TARGETS)
$(BUILD_C_DIR)/%.c: $(SRC_CORE_DIR)/%.cbl
	@mkdir -p $(@D)
	@echo "🛠️  Generando código C de $<"
	$(COBC) $(COBC_FLAGS) -v -C $< -o $@
	@echo "✅ Generado $@"


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
profile: .update-attempt prepare
	@mkdir -p $(PROFILE_REPORT_DIR)
	@echo "🔍 Generando perfil de compilación tipo: $(BUILD_TYPE)" | tee $(PROFILE_REPORT_DIR)/compilation.log
	@echo "Build type: $(BUILD_TYPE)"     >  $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Version: $(VERSION)"          >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Platform: $(PLATFORM)-$(ARCH)" >> $(PROFILE_REPORT_DIR)/build-info.txt
	@echo "Timestamp: $$(date)"          >> $(PROFILE_REPORT_DIR)/build-info.txt

	@{ \
	  if [ "$(BUILD_TYPE)" = "m" ]; then \
	    echo "Invocando: make release-modular" | tee -a $(PROFILE_REPORT_DIR)/compilation.log; \
	    make release-modular 2> >(tee $(PROFILE_REPORT_DIR)/errors.log >&2) \
	                     | tee -a $(PROFILE_REPORT_DIR)/compilation.log; \
	  else \
	    echo "Invocando: make release-single" | tee -a $(PROFILE_REPORT_DIR)/compilation.log; \
	    make release-single 2> >(tee $(PROFILE_REPORT_DIR)/errors.log >&2) \
	                     | tee -a $(PROFILE_REPORT_DIR)/compilation.log; \
	  fi ; \
	  STATUS=$$?; \
	  echo "Exit status: $$STATUS" >> $(PROFILE_REPORT_DIR)/build-info.txt; \
	  exit $$STATUS; \
	}

# Clean Build
clean:
	rm -rf $(BUILD_DIR)
	rm -f .buildattempt
	@echo "🧹 Build cleaned"
