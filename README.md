# gltf-builder

## Running examples

All examples write output to the `created-models/` directory.

Green square — a simple flat square with a solid green color, written as embedded glTF JSON.

```bash
stack exec gltf-builder-exe -- --example square
```

Textured box — a box with a crate texture, written as embedded glTF JSON with the image base64-encoded inside the file.

```bash
stack exec gltf-builder-exe -- --example textured-box
```

Textured box (binary) — the same textured box written as a binary GLB file with geometry and image data packed into a single binary chunk.

```bash
stack exec gltf-builder-exe -- --example textured-box-binary --binary
```

To preview the output, drag and drop the file into the [Babylon.js Sandbox](https://sandbox.babylonjs.com) — it supports both `.gltf` and `.glb`.

### Additional flags

| Flag | Description |
|---|---|
| `--pretty-print` | Pretty-print JSON output |
| `--many-buffers` | One buffer per mesh instead of a single combined buffer |
| `--buffer-images` | Store images in buffer views instead of data URIs |
