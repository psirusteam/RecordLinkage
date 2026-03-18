# RecordLinkage GitHub Pages

Este repositorio quedó preparado para publicar una página de GitHub Pages a partir del archivo `RLteoría.Rmd`.

## Cómo publicarla

1. Sube estos cambios a GitHub.
2. En el repositorio, entra a **Settings > Pages**.
3. En **Build and deployment**, selecciona **GitHub Actions** como fuente.
4. Haz un push a `main` o `master` (o ejecuta manualmente el workflow desde la pestaña **Actions**).

El workflow `.github/workflows/render-gh-pages.yml` renderiza `RLteoría.Rmd` como `index.html` y lo publica automáticamente en GitHub Pages.
