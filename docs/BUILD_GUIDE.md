# HKL Library Build Guide

## Overview

This guide provides comprehensive instructions for building the HKL library from source on various platforms. The HKL library uses the GNU autotools build system for cross-platform compatibility.

## Prerequisites

### Required Dependencies

#### Core Dependencies
- **GCC** >= 4.9 or **Clang** >= 3.4
- **GNU Make** >= 4.0
- **Autotools** (autoconf >= 2.69, automake >= 1.15, libtool >= 2.4)
- **pkg-config** >= 0.28

#### Mathematical Libraries
- **GSL (GNU Scientific Library)** >= 1.12
  - Provides mathematical functions and linear algebra
  - Required for all HKL functionality

#### Core System Libraries
- **GLib** >= 2.0
  - Provides object system, memory management, and utilities
  - Required for all HKL functionality

#### Documentation (Optional but Recommended)
- **gtk-doc** >= 1.9
  - Generates API documentation
  - Required for `--enable-hkl-doc`

### Optional Dependencies

#### GUI Application
- **GTK+** >= 3.22
  - For the graphical interface (ghkl)
  - Required for `--enable-gui`

#### 3D Visualization (HKL3D)
- **OpenGL** >= 1.0
- **GLU** >= 7.7.1
- **Bullet Physics** >= 2.82
- **libg3d** >= 0.0.8
- **libyaml** >= 0.1.0
  - Required for `--enable-hkl3d`

#### Binoculars-NG Application
- **GHC (Glasgow Haskell Compiler)** >= 8.0
- **Cabal** >= 2.0
- **HDF5** >= 1.8.13
- **CGLM** >= 0.7
- **inih** >= 55
  - Required for `--enable-binoculars`

#### Additional Tools
- **Asymptote** (for documentation figures)
- **POV-Ray** (for 3D documentation figures)
- **Gnuplot** (for plotting examples)
- **Python** >= 3.6 with **pyyaml** (for some tools)

## Platform-Specific Setup

### Ubuntu/Debian

```bash
# Update package list
sudo apt update

# Install build tools and autotools
sudo apt install build-essential autotools-dev autoconf automake libtool pkg-config

# Install required dependencies
sudo apt install libgsl-dev libglib2.0-dev gtk-doc-tools

# Install optional dependencies
sudo apt install libgtk-3-dev libhdf5-dev libyaml-dev
sudo apt install libgl1-mesa-dev libglu1-mesa-dev libbullet-dev libg3d-dev

# Install Haskell (for binoculars-ng)
sudo apt install ghc cabal-install

# Install documentation tools
sudo apt install asymptote povray gnuplot python3-yaml
```

### CentOS/RHEL/Fedora

```bash
# Install build tools
sudo dnf groupinstall "Development Tools"
sudo dnf install autotools-dev autoconf automake libtool pkgconfig

# Install required dependencies
sudo dnf install gsl-devel glib2-devel gtk-doc

# Install optional dependencies
sudo dnf install gtk3-devel hdf5-devel libyaml-devel
sudo dnf install mesa-libGL-devel mesa-libGLU-devel bullet-devel

# Install Haskell
sudo dnf install ghc cabal-install

# Install documentation tools
sudo dnf install asymptote povray gnuplot python3-pyyaml
```

### macOS

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install build tools
brew install autoconf automake libtool pkg-config

# Install required dependencies
brew install gsl glib gtk-doc

# Install optional dependencies
brew install gtk+3 hdf5 yaml-cpp
brew install bullet

# Install Haskell
brew install ghc cabal-install

# Install documentation tools
brew install asymptote povray gnuplot pyyaml
```

### Windows (MSYS2/MinGW)

```bash
# Install MSYS2 from https://www.msys2.org/
# Open MSYS2 terminal

# Update package database
pacman -Syu

# Install build tools
pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-make
pacman -S autoconf automake libtool pkg-config

# Install required dependencies
pacman -S mingw-w64-x86_64-gsl mingw-w64-x86_64-glib2 mingw-w64-x86_64-gtk-doc

# Install optional dependencies
pacman -S mingw-w64-x86_64-gtk3 mingw-w64-x86_64-hdf5 mingw-w64-x86_64-yaml-cpp

# Install Haskell
pacman -S mingw-w64-x86_64-ghc cabal-install
```

## Build Process

### 1. Obtaining the Source

```bash
# Clone the repository
git clone https://github.com/synchrotron-soleil/hkl.git
cd hkl

# Or download and extract a release tarball
wget https://github.com/synchrotron-soleil/hkl/releases/download/v5.1.3/hkl-5.1.3.tar.gz
tar -xzf hkl-5.1.3.tar.gz
cd hkl-5.1.3
```

### 2. Bootstrap (for Git Repository)

```bash
# Generate configure script
./autogen.sh
```

### 3. Configuration

#### Basic Configuration
```bash
# Basic build with required components only
./configure
```

#### Full Configuration with All Features
```bash
# Full build with all optional components
./configure --enable-gui --enable-hkl3d --enable-binoculars --enable-hkl-doc
```

#### Development Configuration
```bash
# Development build with debugging and analysis
./configure --enable-logging --enable-analyzer --enable-hkl-doc
```

#### Custom Installation Directory
```bash
# Install to custom prefix
./configure --prefix=/opt/hkl
```

### 4. Build Configuration Options

#### Core Options
- `--enable-gui`: Build GUI application (ghkl)
- `--disable-gui`: Disable GUI application (default: enabled)
- `--enable-hkl3d`: Build 3D visualization library
- `--disable-hkl3d`: Disable 3D visualization (default: disabled)
- `--enable-binoculars`: Build binoculars-ng application
- `--disable-binoculars`: Disable binoculars-ng (default: enabled)

#### Documentation Options
- `--enable-hkl-doc`: Build documentation
- `--disable-hkl-doc`: Disable documentation (default: enabled)
- `--with-mathjax=URL`: Custom MathJax URL for documentation

#### Development Options
- `--enable-logging`: Enable debug logging
- `--disable-logging`: Disable debug logging (default: disabled)
- `--enable-analyzer`: Enable GCC static analyzer
- `--disable-analyzer`: Disable static analyzer (default: disabled)

#### Additional Options
- `--enable-contrib`: Build contrib components
- `--disable-contrib`: Disable contrib components (default: disabled)
- `--enable-datatype99`: Use system datatype99 library
- `--disable-datatype99`: Use bundled datatype99 (default: disabled)

### 5. Compilation

```bash
# Build the project
make

# Parallel build (recommended)
make -j$(nproc)

# Verbose build (for debugging)
make V=1
```

### 6. Testing

```bash
# Run the test suite
make check

# Run tests with verbose output
make check V=1

# Run specific test categories
make -C tests check
make -C binoculars-ng test
```

### 7. Installation

```bash
# Install to system (requires root)
sudo make install

# Install to custom prefix (no root required)
make install

# Update library cache
sudo ldconfig  # Linux
```

## Build Troubleshooting

### Common Issues

#### Missing Dependencies
```bash
# Check for missing pkg-config files
pkg-config --list-all | grep -E "(gsl|glib|gtk)"

# Check specific package
pkg-config --cflags --libs gsl
pkg-config --cflags --libs glib-2.0
```

#### Autotools Issues
```bash
# Regenerate autotools files
autoreconf -fiv

# Clean and restart
make distclean
./autogen.sh
./configure
make
```

#### Haskell/Binoculars Issues
```bash
# Update Cabal
cabal update

# Install Haskell dependencies
cabal install --only-dependencies

# Clean Haskell build
cabal clean
```

#### GTK+ Issues on Linux
```bash
# Install development headers
sudo apt install libgtk-3-dev libglib2.0-dev libpango1.0-dev libcairo2-dev

# Check GTK+ installation
pkg-config --cflags --libs gtk+-3.0
```

#### OpenGL Issues
```bash
# Install OpenGL development packages
sudo apt install mesa-common-dev libgl1-mesa-dev libglu1-mesa-dev

# Check OpenGL installation
pkg-config --cflags --libs gl glu
```

### Platform-Specific Issues

#### macOS Issues
```bash
# Fix library paths
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
export LDFLAGS="-L/usr/local/lib"
export CPPFLAGS="-I/usr/local/include"

# Fix Python path for documentation
export PYTHONPATH="/usr/local/lib/python3.x/site-packages:$PYTHONPATH"
```

#### Windows/MSYS2 Issues
```bash
# Fix PATH for MSYS2
export PATH="/mingw64/bin:$PATH"

# Fix pkg-config path
export PKG_CONFIG_PATH="/mingw64/lib/pkgconfig:$PKG_CONFIG_PATH"
```

### Debugging Build Issues

#### Verbose Output
```bash
# Configure with verbose output
./configure --verbose

# Build with verbose output
make V=1

# Check specific component
make -C hkl V=1
```

#### Dependency Checking
```bash
# Check all dependencies
./configure --help

# Test specific dependency
pkg-config --exists gsl && echo "GSL found" || echo "GSL missing"
```

#### Log Analysis
```bash
# Save build log
make 2>&1 | tee build.log

# Analyze build log
grep -i error build.log
grep -i warning build.log
```

## Custom Builds

### Cross-Compilation

```bash
# Set cross-compilation environment
export CC=x86_64-w64-mingw32-gcc
export CXX=x86_64-w64-mingw32-g++
export PKG_CONFIG=x86_64-w64-mingw32-pkg-config

# Configure for cross-compilation
./configure --host=x86_64-w64-mingw32 --prefix=/opt/hkl-windows
```

### Static Builds

```bash
# Configure for static linking
./configure --enable-static --disable-shared
```

### Debug Builds

```bash
# Configure for debug build
./configure --enable-debug CFLAGS="-g -O0" CXXFLAGS="-g -O0"
```

## Package Creation

### Source Distribution

```bash
# Create source distribution
make dist

# Create distribution with all components
make distcheck
```

### Binary Packages

#### Debian/Ubuntu Package
```bash
# Install packaging tools
sudo apt install devscripts debhelper

# Build package
debuild -us -uc
```

#### RPM Package
```bash
# Create RPM spec file
# Build RPM
rpmbuild -ba hkl.spec
```

## Continuous Integration

### GitHub Actions Example

```yaml
name: Build HKL

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    
    - name: Install dependencies
      run: |
        sudo apt update
        sudo apt install -y build-essential autotools-dev autoconf automake libtool pkg-config
        sudo apt install -y libgsl-dev libglib2.0-dev gtk-doc-tools
        sudo apt install -y libgtk-3-dev libhdf5-dev
    
    - name: Bootstrap
      run: ./autogen.sh
    
    - name: Configure
      run: ./configure --enable-gui --enable-binoculars --enable-hkl-doc
    
    - name: Build
      run: make -j$(nproc)
    
    - name: Test
      run: make check
```

## Maintenance

### Updating Dependencies

```bash
# Update third-party libraries
make ccan-update
make metalang99-update
make datatype99-update
```

### Code Coverage

```bash
# Build with coverage
./configure --enable-coverage CFLAGS="--coverage" LDFLAGS="--coverage"

# Generate coverage report
make lcov
```

### Memory Checking

```bash
# Build with memory checking
./configure CFLAGS="-fsanitize=address" LDFLAGS="-fsanitize=address"

# Run with Valgrind
make check VALGRIND="valgrind --leak-check=full"
```

This build guide should help you successfully compile and install the HKL library on your system. For additional help, consult the project's issue tracker or documentation.
