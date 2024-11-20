import { build, emptyDir } from "@deno/dnt";

await emptyDir("./npm");

const version : string | undefined = JSON.parse(await Deno.readTextFile("deno.json"))['version'];
if (!version) {
  throw new Error("can't get version from deno.json")
}

await build({
  entryPoints: ["./src/mod.ts"],
  outDir: "./npm",
  shims: {
    deno: true,
  },
  importMap: "deno.json",
  package: {
    name: "@adllang/adl-runtime",
    version,
    description: "Runtime support code for the ADL system",
    license: "BSD",
    repository: {
      type: "git",
      url: "git+https://github.com/adl-lang/adl.git",
    },
    bugs: {
      url: "https://github.com/adl-lang/adl/issues",
    },
  },
  postBuild() {
    Deno.copyFileSync("LICENSE", "npm/LICENSE");
    Deno.copyFileSync("README.md", "npm/README.md");
  },
});
