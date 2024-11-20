To publish to jsr

```
deno check src/mod.rs
deno publish                     # publish to jsr
```

To publish to npm

```
deno task build_npm
(cd npm; npm publish --access public)
```
