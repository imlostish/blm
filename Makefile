BUILD_DIR 		:= build
BUILD_C_DIR 	:= $(BUILD_DIR)/c
BUILD_ASM_DIR 	:= $(BUILD_DIR)/asm
BUILD_SBIN_DIR 	:= $(BUILD_DIR)/sbin
BUILD_BIN_DIR 	:= $(BUILD_DIR)/bin

COBC              := cobc
COBC_FLAGS        := -Wmissing-newline -free -Wall

# Cobol Source

SRC_DIR           := src/core
SRC_CONTROLLERS   := $(SRC_DIR)/controllers
SRC_UTILS   	  := $(SRC_DIR)/utils

MAIN_SRC          := $(SRC_DIR)/main.cbl
CLI_SRC           := $(SRC_DIR)/cli.cbl
# CONTROLLERS_SRC   := $(wildcard $(SRC_CONTROLLERS)/*.cbl)

# Obj for modular compilation
OBJ_FILES         := \
	$(BUILD_BIN_DIR)/main.o \
	$(BUILD_BIN_DIR)/cli.o \
	$($(BUILD_BIN_DIR)/%.o, $(CONTROLLERS_SRC))
# patsubst $(SRC_CONTROLLERS)/%.cbl,

SINGLE_BINARY     := $(BUILD_SBIN_DIR)/blm_single
MODULAR_BINARY    := $(BUILD_BIN_DIR)/blm_modular

all: prepare $(SINGLE_BINARY) $(MODULAR_BINARY)

prepare:
	@mkdir -p $(BUILD_C_DIR)
	@mkdir -p $(BUILD_ASM_DIR)
	@mkdir -p $(BUILD_SBIN_DIR)
	@mkdir -p $(BUILD_BIN_DIR)

# -------------------------------------------------------------
# Single Binary Compilation
# -------------------------------------------------------------

$(SINGLE_BINARY): $(MAIN_SRC) $(CLI_SRC)
	$(COBC) $(COBC_FLAGS) -x $^ -o $@
	@echo "✅ Single binary compilation complete → $@"

# -------------------------------------------------------------
# Modular Compilation
# -------------------------------------------------------------

# Compile each .cbl → .o
$(BUILD_BIN_DIR)/main.o: $(MAIN_SRC)
	$(COBC) $(COBC_FLAGS) -c $< -o $@

$(BUILD_BIN_DIR)/cli.o: $(CLI_SRC)
	$(COBC) $(COBC_FLAGS) -c $< -o $@

$(BUILD_BIN_DIR)/%.o: $(SRC_CONTROLLERS)/%.cbl
	$(COBC) $(COBC_FLAGS) -c $< -o $@

# Link all objects → modular binary
$(MODULAR_BINARY): $(OBJ_FILES)
	$(COBC) $(COBC_FLAGS) $^ -x -o $@
	@echo "✅ Modular binary compilation complete → $@"

# -------------------------------------------------------------
# Clean
# -------------------------------------------------------------
clean:
	rm -rf $(BUILD_DIR)
	@echo "🧹 Cleaned build artifacts."

.PHONY: all prepare clean