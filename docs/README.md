# HKL Library Documentation

## Overview

The HKL (H, K, L) library is a comprehensive C library for crystallography and X-ray diffraction calculations, developed by Synchrotron SOLEIL. It provides tools for handling diffractometer geometries, pseudo-axes calculations, sample orientation, and detector data processing.

## Table of Contents

- [Project Overview](#project-overview)
- [Architecture](#architecture)
- [Core Components](#core-components)
- [Build System](#build-system)
- [Applications](#applications)
- [Development](#development)
- [Contributing](#contributing)

## Project Overview

### Purpose
HKL is designed for synchrotron and laboratory X-ray diffraction experiments, providing:
- Diffractometer geometry calculations
- Pseudo-axis transformations (HKL, Q-space, etc.)
- Sample orientation and UB matrix calculations
- Detector data processing and projection
- Multi-language bindings (Python, etc.)

### Version
Current version: 5.1.3 (as of 2025-01-15)

### License
GNU General Public License v3.0

### Authors
- Picca Frédéric-Emmanuel (Maintainer)
- Maria-Teresa Nunez-Pardo-de-Verra
- Jens Krüger
- Oussama Sboui

## Architecture

The HKL library follows a modular architecture with clear separation between:

1. **Core Library** (`hkl/`): Fundamental crystallographic calculations
2. **Applications** (`binoculars-ng/`, `gui/`): User-facing programs
3. **Documentation** (`Documentation/`): Comprehensive guides and API docs
4. **Tests** (`tests/`): Unit and integration tests
5. **Contributions** (`contrib/`): Additional tools and extensions

## Core Components

### Main Library (`hkl/`)

#### Core Data Structures
- **HklVector**: 3D vector operations
- **HklMatrix**: Matrix mathematics for transformations
- **HklQuaternion**: Rotation representations
- **HklParameter**: Configurable parameters with units
- **HklLattice**: Crystal lattice definitions
- **HklSample**: Sample orientation and UB matrices

#### Geometry System
- **HklGeometry**: Diffractometer geometry definitions
- **HklDetector**: Detector configurations
- **HklEngine**: Pseudo-axis calculation engines
- **HklFactory**: Geometry and engine factory system

#### Supported Diffractometers
- Eulerian 4-circle (E4CV)
- Eulerian 6-circle (E6C)
- Kappa 4-circle (K4CV)
- Kappa 6-circle (K6C)
- Z-axis diffractometer
- SOLEIL SIXS geometries
- SOLEIL SIRIUS KAPPA
- SOLEIL SIRIUS TURRET
- PETRA3 geometries
- APS POLAR
- And many more...

#### Pseudo-Axis Engines
- **hkl**: Miller indices (H, K, L)
- **psi**: Sample rotation around Q
- **q**: Q-space coordinates
- **qper_qpar**: Perpendicular/parallel Q components
- **eulerians**: Euler angle representations
- **tth**: 2-theta calculations

### Binoculars-NG (`binoculars-ng/`)

A Haskell-based application for 2D detector data processing:

#### Features
- Project 2D detector images into reciprocal space
- Support for multiple beamlines (SOLEIL, ESRF, etc.)
- HDF5 data processing
- Configurable projections and corrections
- Multi-threaded processing

#### Components
- **C Library**: Core detector processing (`binoculars/`)
- **Haskell Application**: Data pipeline and configuration
- **Data Sources**: HDF5, image files, dummy data
- **Projections**: Q-space, HKL, angle-based projections

### GUI Application (`gui/`)

A GTK-based graphical interface for diffractometer control:

#### Features
- 3D visualization of diffractometers
- Real-time geometry calculations
- Collision detection (with hkl3d)
- Sample and detector configuration
- Trajectory planning

### HKL3D (`hkl3d/`)

Optional 3D visualization and collision detection library:
- OpenGL rendering
- Bullet Physics integration
- COLLADA model support
- Diffractometer visualization

## Build System

### Dependencies

#### Required
- **GSL** >= 1.12: GNU Scientific Library
- **GLib** >= 2.0: Core utilities
- **gtk-doc** >= 1.9: Documentation generation

#### Optional
- **GTK+** >= 3.22: GUI interface
- **HDF5**: Binoculars data processing
- **CGLM**: OpenGL mathematics
- **libyaml**: Configuration files
- **Bullet**: Physics simulation (hkl3d)
- **libg3d**: 3D model loading

### Build Configuration

```bash
# Basic build
./autogen.sh
./configure
make

# Full build with all features
./configure --enable-gui --enable-hkl3d --enable-binoculars --enable-hkl-doc
make

# Development build
./configure --enable-logging --enable-analyzer
make
```

### Build Options
- `--enable-gui`: Build GUI application
- `--enable-hkl3d`: Build 3D visualization library
- `--enable-binoculars`: Build binoculars-ng application
- `--enable-hkl-doc`: Build documentation
- `--enable-logging`: Enable debug logging
- `--enable-analyzer`: Enable GCC static analyzer

## Applications

### Command Line Tools

#### hkl
Basic command-line interface for geometry calculations

#### binoculars-ng
Haskell application for detector data processing:
```bash
# Process data
binoculars-ng process config.ini

# Create new configuration
binoculars-ng cfg-new qxqyqz

# Merge files
binoculars-ng merge -o output.h5 file1.h5 file2.h5
```

#### ghkl
GUI application for interactive diffractometer control

### Python Bindings

The library provides Python bindings through GObject Introspection:

```python
import gi
gi.require_version('Hkl', '5.0')
from gi.repository import Hkl

# Create geometry
factory = Hkl.Factory.get_by_name("E4CV")
geometry = factory.create_new_geometry()
```

## Development

### Code Organization

#### C Library Structure
```
hkl/
├── hkl-*.c          # Core implementations
├── hkl-*-private.h  # Private headers
├── hkl-*.h          # Public headers (via hkl.h)
└── ccan/            # Embedded C utilities
```

#### Haskell Application Structure
```
binoculars-ng/
├── src/             # Haskell source
├── binoculars/      # C library components
├── data/           # Configuration files
└── app/            # Main application
```

### Testing

Comprehensive test suite using C-TAP-Harness:

```bash
# Run all tests
make check

# Run specific test categories
make -C tests check
make -C binoculars-ng test
```

### Documentation

#### API Documentation
- Generated with gtk-doc
- Available in `Documentation/api/`
- Sphinx documentation in `Documentation/sphinx/`

#### Manual Pages
- `ghkl.1`: GUI application manual
- Configuration examples in `data/`

### Coding Standards

#### C Code
- GNU coding standards
- Comprehensive error handling with GError
- Memory management with GLib
- Extensive unit tests

#### Haskell Code
- Standard Haskell style
- Comprehensive type safety
- Property-based testing with QuickCheck

## Contributing

### Getting Started
1. Clone the repository
2. Install dependencies
3. Run `./autogen.sh && ./configure`
4. Build with `make`
5. Run tests with `make check`

### Development Guidelines
- Follow existing code style
- Add comprehensive tests
- Update documentation
- Use meaningful commit messages
- Respect GPL v3 license

### Supported Platforms
- Linux (primary platform)
- macOS (with limitations)
- Windows (via MSYS2/MinGW)

### Reporting Issues
- Use the project's issue tracker
- Provide detailed reproduction steps
- Include system information
- Test with latest version

## Additional Resources

### Documentation
- [API Reference](Documentation/api/)
- [User Guide](Documentation/sphinx/)
- [Configuration Examples](data/)

### Community
- Mailing lists for development discussions
- Issue tracker for bug reports
- Wiki for additional documentation

### Related Projects
- [Synchrotron SOLEIL](https://www.synchrotron-soleil.fr/)
- [CCAN](https://ccodearchive.net/): C utilities used in hkl
- [GObject Introspection](https://gi.readthedocs.io/): Python bindings

---

*This documentation provides a comprehensive overview of the HKL library. For detailed API information, see the generated documentation in the `Documentation/` directory.*
