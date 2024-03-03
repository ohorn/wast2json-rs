# wast2json-rs

A [`wast2json`](https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md) implementation written in Rust.

## Installation

You can download prebuilt binaries from the [Release page](https://github.com/ohorn/wast2json-rs/releases).

You can also install from source using Cargo:

```sh
cargo install wast2json
```

## Usage

The binary file is named `wast2json-rs` to allow installation alongside wabt's `wast2json`.

```sh
$ wast2json-rs wast2json spec-test.wast -o spec-test.json
```

Details about the output JSON format can be found on the wabt [`wast2json` page](https://github.com/WebAssembly/wabt/blob/main/docs/wast2json.md#json-format).
