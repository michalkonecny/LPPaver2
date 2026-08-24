# Coding Conventions

## Import Ordering

Order imports from most general to most specific:

1. External packages (`pinia`, `vue`, `lodash`, `plotly.js-dist-min`, etc.)
2. `@/` alias imports (project-wide, cross-folder)
3. `../` relative imports (parent/sibling folders)
4. `./` relative imports (same folder — most specific)
