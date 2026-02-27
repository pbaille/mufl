# MUFL Project Structure

## Top-Level Contents

### Configuration & Metadata
- **.gitignore** — Git ignore rules
- **deps.edn** — Clojure/ClojureCLI dependency and build configuration

### Source Code
- **src/** — Main source directory (Clojure code)
- **test/** — Test directory (Clojure tests)

### Documentation
- **doc/** — Documentation files

### Git & Version Control
- **.git/** — Git repository metadata

### Development Tools
- **.clj-kondo/** — Clojure linter configuration and cache
- **.lsp/** — Language Server Protocol cache/configuration
- **.cpcache/** — Clojure CLI cache directory
- **.pi/** — Pi agent configuration and journal

---

**Summary:** This is a Clojure project using CLI (deps.edn) for dependency management. The structure follows standard Clojure conventions with separate src/ and test/ directories, complemented by documentation in doc/ and tooling configs for linting (clj-kondo) and LSP support.
