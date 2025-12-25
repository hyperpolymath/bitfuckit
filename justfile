# SPDX-License-Identifier: AGPL-3.0-or-later
# justfile for bitfuckit

# Default recipe
default: build

# Build the project
build:
    gprbuild -P bitfuckit.gpr

# Clean build artifacts
clean:
    gprclean -P bitfuckit.gpr

# Build with optimizations
release:
    gprbuild -P bitfuckit.gpr -XBUILD_MODE=release

# Run SPARK verification
verify:
    gnatprove -P bitfuckit.gpr --mode=check

# Install to ~/.local/bin
install: build
    cp bin/bitfuckit ~/.local/bin/

# Install system-wide
install-system: build
    sudo cp bin/bitfuckit /usr/local/bin/

# Uninstall from ~/.local/bin
uninstall:
    rm -f ~/.local/bin/bitfuckit

# Run smoke tests
test: build
    bin/bitfuckit --help
    bin/bitfuckit auth status || true

# Format check (Ada doesn't have a standard formatter, manual review)
fmt:
    @echo "Ada formatting is manual - check style guide"

# Show version
version:
    @grep -m1 "version" STATE.scm | head -1

# Create a new release tag
tag version:
    git tag -s v{{version}} -m "Release v{{version}}"
    git push --tags

# Build man page
man:
    @echo "Man page in doc/bitfuckit.1"

# Generate shell completions
completions:
    @echo "Completions in completions/"

# ============================================================================
# Forge Mesh Operations (Multi-forge mirroring with fallback)
# ============================================================================

# Check health of all forges
forge-health:
    ./scripts/forge-mesh.sh health

# Mirror to all forges (with automatic fallback if GitHub down)
forge-mirror repo:
    ./scripts/forge-mesh.sh mirror {{repo}}

# Sync to specific forge
forge-sync repo dest:
    ./scripts/forge-mesh.sh sync {{repo}} {{dest}}

# Recover from degraded mode (re-sync all from GitHub)
forge-recover repo:
    ./scripts/forge-mesh.sh recover {{repo}}

# Mirror this repo (bitfuckit) to all forges
mirror-self:
    ./scripts/forge-mesh.sh mirror bitfuckit
