# install locations
PREFIX = $(HOME)/INSTALL/harec/latest
BINDIR = $(PREFIX)/bin

# variables used during build
PLATFORM = linux
ARCH = x86_64
HARECFLAGS =
QBEFLAGS =
ASFLAGS =
LDLINKFLAGS = --gc-sections -z noexecstack
CFLAGS = -g -std=c11 -D_XOPEN_SOURCE=700 -Iinclude \
	-Wall -Wextra -Werror -pedantic -Wno-unused-parameter
LDFLAGS =
LIBS = -lm

# commands used by the build script
CC = cc
AS = as
LD = ld
QBE = qbe

# build locations
HARECACHE = .cache
BINOUT = .bin

# variables that will be embedded in the binary with -D definitions
DEFAULT_TARGET = $(ARCH)
VERSION = $$(./scripts/version)
