# carp-wasm
This crate is a small shim to expose JS bindings and enable engine use in the browser. It's meant to
apply minimal changes to the underlying engine: we only replace `std::time` with `web-time`, which
will re-export `std::time` when not targeting WASM.

## Building carp-wasm
Building and using the `carp-wasm` package requires:
 * The `wasm32-unkown-unkown` Cargo target
 * `wasm-pack`
 * A browser with `simd128` support (most [do](https://caniuse.com/?search=simd))

From the root of the repository, you can build `carp-wasm` with:
```
wasm-pack build crates/wasm --target web --out-dir ../../carp-wasm --out-name carp-wasm
```

>**Note:** _If you are doing this for your own crate, remove `strip=true` from `cargo.toml`!!_

## Using carp-wasm
You can find a full working example in my [portfolio website](https://github.com/dede1751/dede1751.github.io).
If you just need to get things going:

```html
<!doctype html>
<html lang="en">
  <meta charset="utf-8" />
  <script type="module">
    import init, { CarpEngine } from "./carp-wasm/carp-wasm.js";

    window.update_perft_data = (data) => console.log("PERFT:", data.asObject());
    window.update_search_data = (data) => console.log("SEARCH:", data.asObject());
    window.update_engine_pick = (data) => console.log("ENGINE PICK:", data);

    await init();
    const engine = new CarpEngine(256); // TT size in MB
    engine.perft("startpos", 7);
    engine.search("startpos moves e2e4 c7c5", "depth 20");
  </script>
</html>
```
