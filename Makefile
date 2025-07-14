# Makefile corregido
BUILD_DIR        := build
BUILD_C_DIR      := $(BUILD_DIR)/c
BUILD_ASM_DIR    := $(BUILD_DIR)/asm
BUILD_SBIN_DIR   := $(BUILD_DIR)/sbin
BUILD_BIN_DIR    := $(BUILD_DIR)/bin

BUILD_BIN_CONTROLLERS_DIR := $(BUILD_BIN_DIR)/controllers
BUILD_BIN_AUTH_DIR := $(BUILD_BIN_DIR)/auth
BUILD_BIN_UTILS_DIR := $(BUILD_BIN_DIR)/utils

COBC             := cobc
COBC_FLAGS       := -Wmissing-newline -free -Wall -I src/core/copybook

# Source
SRC_DIR          := src/core
SRC_CONTROLLERS  := $(SRC_DIR)/controllers
SRC_UTILS   	  := $(SRC_DIR)/utils
SRC_AUTH         := $(SRC_DIR)/auth

MAIN_SRC         := $(SRC_DIR)/main.cbl
CLI_SRC          := $(SRC_DIR)/cli.cbl
CONTROLLERS_SRC  := $(wildcard $(SRC_CONTROLLERS)/*.cbl)
AUTH_SRC         := $(wildcard $(SRC_AUTH)/*.cbl)
UTILS_SRC		 := $(wildcard $(SRC_UTILS)/*cbl)

# Object Files
OBJ_FILES := \
    $(BUILD_BIN_DIR)/main.o \
    $(BUILD_BIN_DIR)/cli.o \
    $(patsubst $(SRC_CONTROLLERS)/%.cbl, $(BUILD_BIN_CONTROLLERS_DIR)/%.o, $(CONTROLLERS_SRC)) \
    $(patsubst $(SRC_AUTH)/%.cbl, $(BUILD_BIN_AUTH_DIR)/%.o, $(AUTH_SRC)) \
	$(patsubst $(SRC_UTILS)/%.cbl, $(BUILD_BIN_UTILS_DIR)/%.o, $(UTILS_SRC))

SINGLE_BINARY    := $(BUILD_SBIN_DIR)/blm_single
MODULAR_BINARY   := $(BUILD_BIN_DIR)/blm

all: prepare single modular

prepare:
    @mkdir -p $(BUILD_C_DIR)
    @mkdir -p $(BUILD_ASM_DIR)
    @mkdir -p $(BUILD_SBIN_DIR)
    @mkdir -p $(BUILD_BIN_DIR)
    @mkdir -p $(BUILD_BIN_CONTROLLERS_DIR)
    @mkdir -p $(BUILD_BIN_AUTH_DIR)
    @mkdir -p $(BUILD_BIN_UTILS_DIR)

# -------------------------------------------------------------
# Single Binary Compilation
# -------------------------------------------------------------

single: $(SINGLE_BINARY)

$(SINGLE_BINARY): $(MAIN_SRC) $(CLI_SRC) $(CONTROLLERS_SRC) $(AUTH_SRC)
    $(COBC) $(COBC_FLAGS) -x $^ -o $@
    @echo "✅ Single binary: $@"

# -------------------------------------------------------------
# Modular Compilation
# -------------------------------------------------------------

modular: $(MODULAR_BINARY)

# Compile every .cbl → .o
$(BUILD_BIN_DIR)/main.o: $(MAIN_SRC)
    $(COBC) $(COBC_FLAGS) -c $< -o $@

$(BUILD_BIN_DIR)/cli.o: $(CLI_SRC)
    $(COBC) $(COBC_FLAGS) -c $< -o $@

$(BUILD_BIN_CONTROLLERS_DIR)/%.o: $(SRC_CONTROLLERS)/%.cbl
    $(COBC) $(COBC_FLAGS) -c $< -o $@

$(BUILD_BIN_AUTH_DIR)/%.o: $(SRC_AUTH)/%.cbl
    $(COBC) $(COBC_FLAGS) -c $< -o $@

# Link objects
$(MODULAR_BINARY): $(OBJ_FILES)
    $(COBC) $(COBC_FLAGS) -x $^ -o $@
    @echo "✅ Modular binary: $@"

# -------------------------------------------------------------
# Clean
# -------------------------------------------------------------
clean:
    rm -rf $(BUILD_DIR)
    @echo "🧹 Build cleaned"

.PHONY: all prepare single modular clean