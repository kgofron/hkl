# HKL Library Architecture

## Overview

The HKL library is designed with a modular, object-oriented architecture in C, providing a clean separation between core crystallographic calculations, geometry management, and user interfaces.

## Core Architecture Principles

### 1. Object-Oriented Design in C
- Uses GLib's GObject system for object management
- Clear inheritance hierarchies for geometries and engines
- Polymorphic interfaces for different diffractometer types

### 2. Separation of Concerns
- **Core Math**: Vector, matrix, quaternion operations
- **Geometry**: Diffractometer-specific implementations
- **Engines**: Pseudo-axis calculation algorithms
- **Applications**: User-facing programs and GUIs

### 3. Extensibility
- Factory pattern for adding new geometries
- Plugin-like engine system
- Configurable parameters and units

## Component Architecture

### Core Library (`hkl/`)

```
┌─────────────────────────────────────────────────────────┐
│                    Public API (hkl.h)                  │
├─────────────────────────────────────────────────────────┤
│  Vector │ Matrix │ Quaternion │ Parameter │ Unit        │
│    │        │         │          │         │            │
├─────────────────────────────────────────────────────────┤
│              Geometry System                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Geometry   │  │  Detector   │  │   Sample    │     │
│  │             │  │             │  │             │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                 Engine System                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │   HKL       │  │     Q       │  │    Psi      │     │
│  │  Engine     │  │   Engine    │  │   Engine    │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                Factory System                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Geometry   │  │   Engine    │  │  Detector   │     │
│  │  Factory    │  │   Factory   │  │  Factory    │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└─────────────────────────────────────────────────────────┘
```

### Data Flow

```
Sample Definition → Geometry Setup → Engine Selection → Calculation
       │                │                │              │
   Lattice + UB    Diffractometer    Pseudo-axes    Results
   Matrix          Configuration     Algorithms     (HKL, Q, etc.)
```

## Detailed Component Analysis

### 1. Core Mathematical Components

#### HklVector
```c
struct _HklVector {
    double data[3];
};
```
- 3D vector operations for crystallographic calculations
- Unit vectors for coordinate system definitions
- Basic arithmetic operations (add, subtract, scale, dot product, cross product)

#### HklMatrix
```c
struct _HklMatrix {
    double data[9];  // 3x3 matrix in row-major order
};
```
- 3x3 transformation matrices
- Rotation matrices from Euler angles
- UB matrix calculations for sample orientation
- Matrix operations (multiplication, inversion, determinant)

#### HklQuaternion
```c
struct _HklQuaternion {
    double data[4];  // [w, x, y, z]
};
```
- Rotation representations avoiding gimbal lock
- Conversion to/from matrices
- Smooth interpolation between orientations

### 2. Geometry System

#### HklGeometry
Central component managing diffractometer configuration:

```c
typedef struct _HklGeometry HklGeometry;

// Key functions:
HklGeometry *hkl_geometry_new_copy(const HklGeometry *self);
const darray_string *hkl_geometry_axis_names_get(const HklGeometry *self);
const HklParameter *hkl_geometry_axis_get(const HklGeometry *self, const char *name, GError **error);
HklVector hkl_geometry_ki_get(const HklGeometry *self);  // Incident beam vector
HklVector hkl_geometry_kf_get(const HklGeometry *self, const HklDetector *detector);
```

#### Supported Geometries
Each geometry implements specific diffractometer configurations:

- **E4CV (Eulerian 4-Circle Vertical)**: Standard 4-circle diffractometer
- **E6C (Eulerian 6-Circle)**: 6-circle diffractometer with additional axes
- **K4CV (Kappa 4-Circle Vertical)**: Kappa geometry with 4 circles
- **K6C (Kappa 6-Circle)**: Kappa geometry with 6 circles
- **ZAxis**: Z-axis geometry for reflectivity measurements
- **SOLEIL Geometries**: Specialized geometries for SOLEIL beamlines

### 3. Engine System

#### HklEngine
Abstract base class for pseudo-axis calculations:

```c
typedef struct _HklEngine HklEngine;

// Core engine interface:
const char *hkl_engine_name_get(const HklEngine *self);
const darray_string *hkl_engine_pseudo_axis_names_get(HklEngine *self);
int hkl_engine_pseudo_axis_values_get(HklEngine *self, double values[], size_t n_values, 
                                     HklUnitEnum unit_type, GError **error);
HklGeometryList *hkl_engine_pseudo_axis_values_set(HklEngine *self, double values[], 
                                                  size_t n_values, HklUnitEnum unit_type, 
                                                  GError **error);
```

#### Engine Types

**HKL Engine**: Miller indices calculations
- Converts between diffractometer angles and H, K, L indices
- Handles different modes (bissector, constant_omega, etc.)
- Multiple solutions for multi-circle geometries

**Q Engine**: Q-space calculations
- Converts to reciprocal space coordinates
- Handles different Q-space representations
- Supports custom Q-space projections

**Psi Engine**: Sample rotation
- Rotates sample around scattering vector Q
- Important for texture measurements
- Derived from Eulerian geometries

### 4. Sample Management

#### HklSample
Manages crystal sample properties:

```c
typedef struct _HklSample HklSample;

// Key functions:
HklSample *hkl_sample_new(const char *name);
const HklLattice *hkl_sample_lattice_get(HklSample *self);
const HklMatrix *hkl_sample_UB_get(const HklSample *self);
int hkl_sample_UB_set(HklSample *self, const HklMatrix *UB, GError **error);
```

#### HklLattice
Crystal lattice parameters:

```c
typedef struct _HklLattice HklLattice;

// Lattice parameters (a, b, c, alpha, beta, gamma)
const HklParameter *hkl_lattice_a_get(const HklLattice *self);
int hkl_lattice_get_B(const HklLattice *self, HklMatrix *B);  // B matrix
```

### 5. Factory System

#### HklFactory
Creates geometries and engines:

```c
typedef struct _HklFactory HklFactory;

HklFactory **hkl_factory_get_all(size_t *n);
HklFactory *hkl_factory_get_by_name(const char *name, GError **error);
HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self);
HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self);
```

## Application Architecture

### Binoculars-NG Application

#### Haskell Layer
```
┌─────────────────────────────────────────────────────────┐
│                 Main Application                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │    CLI      │  │   Config    │  │  Pipeline   │     │
│  │  Parser     │  │ Management  │  │ Management  │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                Processing Pipeline                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │ Data Source │  │ Projection  │  │   Output    │     │
│  │  (HDF5,Img) │  │  (HKL,Q)    │  │   (HDF5)    │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                 C Library Interface                     │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Detector   │  │  Geometry   │  │  Sample     │     │
│  │ Processing  │  │  Handling   │  │  Handling   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└─────────────────────────────────────────────────────────┘
```

#### C Library Components
- **Detector Processing**: 2D detector data handling
- **Geometry Management**: Diffractometer configuration
- **I/O Operations**: HDF5, image file support
- **Mathematical Operations**: Coordinate transformations

### GUI Application Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    GTK Interface                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │   Main      │  │   3D View   │  │  Pseudo     │     │
│  │  Window     │  │   (OpenGL)  │  │  Axes       │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                 HKL Library Interface                   │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Geometry   │  │   Engine    │  │   Sample    │     │
│  │  Control    │  │   Control   │  │   Control   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
├─────────────────────────────────────────────────────────┤
│                HKL3D Library (Optional)                 │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │  Collision  │  │   Model     │  │ Rendering   │     │
│  │ Detection   │  │  Loading    │  │  (OpenGL)   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└─────────────────────────────────────────────────────────┘
```

## Memory Management

### GLib Integration
- **GObject**: Reference counting for objects
- **GError**: Comprehensive error handling
- **GQuark**: Error domain definitions
- **GType**: Runtime type system

### Memory Safety
- Automatic reference counting
- Clear ownership semantics
- Comprehensive error propagation
- Memory leak detection in debug builds

## Error Handling

### Error Propagation
```c
// Standard error handling pattern:
GError *error = NULL;
HklGeometry *geom = hkl_geometry_new_copy(other, &error);
if (error != NULL) {
    // Handle error
    g_error_free(error);
    return;
}
```

### Error Domains
- `HKL_GEOMETRY_ERROR`: Geometry-related errors
- `HKL_ENGINE_ERROR`: Engine calculation errors
- `HKL_SAMPLE_ERROR`: Sample-related errors
- `HKL_PARAMETER_ERROR`: Parameter validation errors

## Thread Safety

### Design Considerations
- Most operations are thread-safe for read access
- Write operations require external synchronization
- Factory objects are immutable and thread-safe
- Geometry and engine objects should be used from single thread

### Parallel Processing
- Binoculars-NG uses Haskell's parallel processing capabilities
- C library designed to be called from multiple threads
- No global state in core calculations

## Extensibility Points

### Adding New Geometries
1. Create new geometry implementation file (`hkl-engine-*.c`)
2. Implement required geometry functions
3. Register with factory system
4. Add to build system

### Adding New Engines
1. Create engine implementation file
2. Implement engine interface
3. Register with engine factory
4. Add configuration support

### Adding New Detectors
1. Implement detector interface
2. Add detector-specific calculations
3. Register with detector factory
4. Update documentation

## Performance Considerations

### Optimization Strategies
- **Lazy Evaluation**: Calculations performed only when needed
- **Caching**: Results cached for repeated operations
- **Vectorization**: GSL integration for optimized math operations
- **Memory Pooling**: Efficient memory allocation patterns

### Profiling Support
- Built-in timing capabilities
- Memory usage tracking
- Performance counters for critical paths

## Build System Integration

### Autotools Integration
- Standard GNU autotools build system
- Conditional compilation based on available dependencies
- Cross-platform support
- Integration with pkg-config

### Library Organization
- **libhkl**: Core library
- **libhkl-binoculars**: Detector processing
- **libhkl3d**: 3D visualization (optional)
- **hkl-binoculars-ng**: Haskell application
- **ghkl**: GUI application

This architecture provides a solid foundation for crystallographic calculations while maintaining flexibility for different experimental setups and future extensions.
