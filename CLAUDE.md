# Architecture

## Layer rule
Source directories form strict layers — dependencies flow downward only:

  Geometry → Core → Gltf

- `Gltf/` — lowest layer: glTF JSON types, binary GLB format, encoding primitives
- `Core/` — middle layer: scene model, DSL, high-level encoding pipeline
- `Geometry/` — highest layer: geometric shapes and utilities

A module in a lower layer must never import from a higher layer.

The `Lib/` directory is a shared utilities module — it may be imported by any layer.
