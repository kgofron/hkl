# HKL Library Development Guide

## Overview

This guide provides information for developers working on the HKL library, including coding standards, testing procedures, contribution guidelines, and maintenance tasks.

## Table of Contents

1. [Development Environment](#development-environment)
2. [Coding Standards](#coding-standards)
3. [Testing](#testing)
4. [Documentation](#documentation)
5. [Contributing](#contributing)
6. [Maintenance](#maintenance)
7. [Release Process](#release-process)

## Development Environment

### Required Tools

#### Build Tools
- **GCC** >= 4.9 or **Clang** >= 3.4
- **GNU Make** >= 4.0
- **Autotools** (autoconf >= 2.69, automake >= 1.15, libtool >= 2.4)
- **pkg-config** >= 0.28

#### Development Dependencies
- **GSL** >= 1.12 (libgsl-dev)
- **GLib** >= 2.0 (libglib2.0-dev)
- **gtk-doc** >= 1.9 (for API documentation)
- **Valgrind** (for memory checking)
- **GDB** (for debugging)

#### Optional Development Tools
- **Haskell** (for binoculars-ng development)
- **GTK+** >= 3.22 (for GUI development)
- **OpenGL** (for 3D visualization development)

### Development Setup

```bash
# Clone repository
git clone https://github.com/synchrotron-soleil/hkl.git
cd hkl

# Install development dependencies
sudo apt install build-essential autotools-dev autoconf automake libtool pkg-config
sudo apt install libgsl-dev libglib2.0-dev gtk-doc-tools valgrind gdb

# Bootstrap and configure
./autogen.sh
./configure --enable-logging --enable-analyzer --enable-hkl-doc

# Build with debugging symbols
make CFLAGS="-g -O0 -Wall -Wextra"
```

### IDE Configuration

#### VS Code
```json
{
    "configurations": [
        {
            "name": "HKL Debug",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/tests/hkl-test",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "gdb",
            "setupCommands": [
                {
                    "description": "Enable pretty-printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ],
            "preLaunchTask": "build-hkl",
            "miDebuggerPath": "/usr/bin/gdb"
        }
    ]
}
```

#### Emacs
```elisp
;; Add to .emacs
(add-to-list 'load-path "/path/to/hkl")
(require 'hkl-default)
```

## Coding Standards

### C Code Standards

#### Style Guidelines
- **Indentation**: 4 spaces (no tabs)
- **Line Length**: Maximum 80 characters
- **Braces**: K&R style (opening brace on same line)
- **Naming**: snake_case for functions and variables, UPPER_CASE for constants

```c
// Good example
static int hkl_geometry_calculate_angles(const HklGeometry *self,
                                        const HklVector *q,
                                        double angles[],
                                        GError **error)
{
    if (!self || !q || !angles) {
        g_set_error(error, HKL_GEOMETRY_ERROR, HKL_GEOMETRY_ERROR_INVALID_PARAMETERS,
                   "Invalid parameters");
        return FALSE;
    }
    
    // Implementation
    return TRUE;
}
```

#### Error Handling
- Always use `GError` for error propagation
- Check all function parameters for NULL
- Provide meaningful error messages
- Use appropriate error domains and codes

```c
// Error domain definition
#define HKL_GEOMETRY_ERROR hkl_geometry_error_quark()

GQuark hkl_geometry_error_quark(void)
{
    return g_quark_from_static_string("hkl-geometry-error");
}

typedef enum {
    HKL_GEOMETRY_ERROR_INVALID_PARAMETERS,
    HKL_GEOMETRY_ERROR_CALCULATION_FAILED,
    HKL_GEOMETRY_ERROR_NO_SOLUTIONS
} HklGeometryError;
```

#### Memory Management
- Use GLib memory management functions (`g_malloc`, `g_free`)
- Implement reference counting for complex objects
- Always free resources in error paths
- Use `g_autofree` for automatic cleanup

```c
// Good memory management
HklGeometry *hkl_geometry_new_with_params(double wavelength,
                                         const char *name,
                                         GError **error)
{
    g_autofree HklGeometry *geometry = NULL;
    g_autofree char *geometry_name = NULL;
    
    geometry = g_new0(HklGeometry, 1);
    geometry_name = g_strdup(name);
    
    // Initialize geometry
    geometry->wavelength = wavelength;
    geometry->name = g_steal_pointer(&geometry_name);
    
    return g_steal_pointer(&geometry);
}
```

#### Documentation
- Use Doxygen-style comments for public functions
- Document all parameters and return values
- Provide usage examples for complex functions

```c
/**
 * hkl_geometry_set_wavelength:
 * @self: (not nullable): A #HklGeometry
 * @wavelength: The wavelength value
 * @unit_type: The unit type (default or user)
 * @error: (nullable): Return location for a #GError, or %NULL
 *
 * Sets the wavelength of the geometry. The wavelength must be positive.
 *
 * Returns: %TRUE on success, %FALSE on error
 *
 * Since: 5.0
 */
gboolean hkl_geometry_set_wavelength(HklGeometry *self,
                                    double wavelength,
                                    HklUnitEnum unit_type,
                                    GError **error);
```

### Haskell Code Standards

#### Style Guidelines
- Follow standard Haskell style guidelines
- Use meaningful function and variable names
- Prefer explicit type signatures for exported functions
- Use `hlint` for style checking

```haskell
-- Good example
processDetectorData :: MonadIO m 
                    => DetectorConfig 
                    -> FilePath 
                    -> m (Either String ProcessedData)
processDetectorData config inputPath = do
    result <- liftIO $ withFile inputPath ReadMode $ \handle -> do
        rawData <- readDetectorData handle
        pure $ processData config rawData
    
    case result of
        Left err -> pure $ Left $ "Processing failed: " ++ err
        Right data' -> pure $ Right data'
```

#### Error Handling
- Use `Either` for error handling in pure code
- Use `MonadThrow` for monadic error handling
- Provide meaningful error messages

```haskell
-- Error handling example
data ProcessingError = FileNotFound FilePath
                     | InvalidFormat String
                     | ProcessingFailed String
    deriving (Show, Eq)

processFile :: MonadThrow m => FilePath -> m ProcessedData
processFile path = do
    exists <- liftIO $ doesFileExist path
    unless exists $ throwM $ FileNotFound path
    
    content <- liftIO $ readFile path
    case parseData content of
        Left err -> throwM $ InvalidFormat err
        Right data' -> pure data'
```

## Testing

### Test Framework

The HKL library uses **C-TAP-Harness** for unit testing, providing a lightweight but comprehensive testing framework.

#### Running Tests

```bash
# Run all tests
make check

# Run specific test categories
make -C tests check
make -C binoculars-ng test

# Run with verbose output
make check V=1

# Run with memory checking
make check VALGRIND="valgrind --leak-check=full"
```

#### Writing Tests

```c
#include "tap/basic.h"
#include <hkl.h>

int main(void)
{
    plan(10);  // Plan number of tests
    
    // Test 1: Basic object creation
    GError *error = NULL;
    HklGeometry *geometry = hkl_geometry_new_copy(NULL, &error);
    ok(geometry != NULL, "Geometry creation succeeds");
    ok(error == NULL, "No error on successful creation");
    
    // Test 2: Error handling
    HklGeometry *invalid = hkl_geometry_new_copy(NULL, &error);
    ok(invalid == NULL, "Invalid creation returns NULL");
    ok(error != NULL, "Error is set on failure");
    
    // Cleanup
    if (geometry) hkl_geometry_free(geometry);
    if (error) g_error_free(error);
    
    return exit_status();
}
```

#### Test Categories

1. **Unit Tests**: Individual function testing
2. **Integration Tests**: Component interaction testing
3. **Regression Tests**: Bug reproduction and verification
4. **Performance Tests**: Benchmarking critical paths

#### Test Data

```bash
# Generate test data
make -C tests generate-test-data

# Validate test data
make -C tests validate-test-data
```

### Continuous Integration

#### GitHub Actions Configuration

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        config: [basic, full, debug]
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install dependencies
      run: |
        sudo apt update
        sudo apt install -y build-essential autotools-dev autoconf automake libtool pkg-config
        sudo apt install -y libgsl-dev libglib2.0-dev gtk-doc-tools
        if [ "${{ matrix.config }}" = "full" ]; then
          sudo apt install -y libgtk-3-dev libhdf5-dev
        fi
    
    - name: Bootstrap
      run: ./autogen.sh
    
    - name: Configure
      run: |
        case "${{ matrix.config }}" in
          basic) ./configure ;;
          full) ./configure --enable-gui --enable-binoculars --enable-hkl-doc ;;
          debug) ./configure --enable-logging --enable-analyzer CFLAGS="-g -O0" ;;
        esac
    
    - name: Build
      run: make -j$(nproc)
    
    - name: Test
      run: make check
    
    - name: Memory check
      if: matrix.config == 'debug'
      run: make check VALGRIND="valgrind --leak-check=full --error-exitcode=1"
```

## Documentation

### API Documentation

#### Generating Documentation

```bash
# Generate API documentation
make -C Documentation/api docs

# Generate all documentation
make -C Documentation all
```

#### Documentation Standards

- Use **gtk-doc** for C API documentation
- Follow **Doxygen** conventions for comments
- Include usage examples in documentation
- Keep documentation up-to-date with code changes

```c
/**
 * SECTION:hkl-geometry
 * @short_description: Diffractometer geometry management
 * @title: HklGeometry
 * @include: hkl.h
 *
 * The #HklGeometry object represents a specific diffractometer
 * configuration with axes, wavelength, and detector settings.
 *
 * ## Example
 * |[
 * HklGeometry *geometry = hkl_geometry_new_copy(NULL, &error);
 * hkl_geometry_wavelength_set(geometry, 1.0, HKL_UNIT_DEFAULT, &error);
 * ]|
 */

/**
 * hkl_geometry_new_copy:
 * @src: (nullable): Source geometry to copy, or %NULL for default
 * @error: (nullable): Return location for a #GError, or %NULL
 *
 * Creates a new #HklGeometry object. If @src is provided, the new
 * geometry will be a copy of the source geometry.
 *
 * Returns: (transfer full): A new #HklGeometry, or %NULL on error
 *
 * Since: 5.0
 */
HklGeometry *hkl_geometry_new_copy(const HklGeometry *src, GError **error);
```

### User Documentation

#### Sphinx Documentation

```bash
# Generate Sphinx documentation
make -C Documentation/sphinx html

# Serve documentation locally
cd Documentation/sphinx/_build/html
python3 -m http.server 8000
```

#### Writing User Guides

- Use clear, concise language
- Provide practical examples
- Include troubleshooting sections
- Keep guides up-to-date with API changes

## Contributing

### Contribution Process

1. **Fork** the repository
2. **Create** a feature branch
3. **Implement** changes with tests
4. **Update** documentation
5. **Submit** a pull request

### Pull Request Guidelines

#### Required Elements
- **Description**: Clear explanation of changes
- **Tests**: New or updated tests
- **Documentation**: Updated API docs and user guides
- **Changelog**: Entry in NEWS file

#### Code Review Checklist
- [ ] Code follows style guidelines
- [ ] All tests pass
- [ ] Documentation is updated
- [ ] No memory leaks or errors
- [ ] Performance impact is acceptable

### Commit Guidelines

#### Commit Message Format
```
type(scope): brief description

Detailed explanation of changes, including:
- What was changed
- Why it was changed
- Any breaking changes

Closes #issue_number
```

#### Types
- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation changes
- **style**: Code style changes
- **refactor**: Code refactoring
- **test**: Test additions/changes
- **chore**: Maintenance tasks

#### Examples
```
feat(geometry): add support for new diffractometer type

Adds support for the new SOLEIL SIRIUS TURRET geometry,
including all required axes and engine configurations.

Closes #123
```

```
fix(engine): resolve memory leak in HKL engine

Fixes memory leak that occurred when multiple solutions
were generated but not all were used.

Fixes #456
```

## Maintenance

### Code Quality

#### Static Analysis

```bash
# Run static analysis
make analyzer

# Run clang static analyzer
scan-build make

# Run cppcheck
cppcheck --enable=all --inconclusive --std=c99 hkl/
```

#### Code Coverage

```bash
# Build with coverage
./configure --enable-coverage CFLAGS="--coverage" LDFLAGS="--coverage"

# Generate coverage report
make lcov

# View coverage report
open hkl-lcov/index.html
```

#### Memory Checking

```bash
# Run with Valgrind
make check VALGRIND="valgrind --leak-check=full --show-leak-kinds=all"

# Run with AddressSanitizer
./configure CFLAGS="-fsanitize=address" LDFLAGS="-fsanitize=address"
make check
```

### Dependency Management

#### Updating Third-Party Libraries

```bash
# Update CCAN library
make ccan-update

# Update Metalang99
make metalang99-update

# Update Datatype99
make datatype99-update
```

#### Security Updates

- Monitor security advisories for dependencies
- Update dependencies regularly
- Test thoroughly after updates
- Document any breaking changes

### Performance Monitoring

#### Benchmarking

```bash
# Run benchmarks
make -C tests benchmark

# Profile critical functions
perf record --call-graph dwarf ./tests/hkl-benchmark
perf report
```

#### Performance Regression Testing

```bash
# Run performance tests
make -C tests perf-check

# Compare with baseline
make -C tests perf-compare BASELINE=v5.1.2
```

## Release Process

### Version Management

#### Version Numbering
- **Major**: Breaking API changes
- **Minor**: New features, backward compatible
- **Micro**: Bug fixes, backward compatible
- **Revision**: Development snapshots

#### Release Branching
```bash
# Create release branch
git checkout -b release-5.2.0

# Update version numbers
sed -i 's/5.1.3/5.2.0/g' configure.ac

# Update NEWS file
# Commit changes
git commit -am "Release 5.2.0"

# Tag release
git tag -a v5.2.0 -m "Release 5.2.0"
```

### Release Checklist

#### Pre-Release
- [ ] All tests pass
- [ ] Documentation is complete
- [ ] Performance is acceptable
- [ ] Security review completed
- [ ] Dependencies are up-to-date

#### Release Process
- [ ] Create release branch
- [ ] Update version numbers
- [ ] Update NEWS and ChangeLog
- [ ] Build and test release candidate
- [ ] Create release tag
- [ ] Generate distribution tarball
- [ ] Upload to release server

#### Post-Release
- [ ] Announce release
- [ ] Update website
- [ ] Monitor for issues
- [ ] Plan next release

### Distribution

#### Source Distribution

```bash
# Create source tarball
make dist

# Verify distribution
make distcheck

# Sign distribution
gpg --armor --detach-sign hkl-5.2.0.tar.gz
```

#### Package Building

```bash
# Build Debian package
debuild -us -uc

# Build RPM package
rpmbuild -ba hkl.spec

# Build Windows installer
makensis hkl.nsi
```

This development guide provides comprehensive information for HKL library developers. For additional information, consult the project's issue tracker, mailing lists, and documentation.
