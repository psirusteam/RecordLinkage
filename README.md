# RecordLinkage GitHub Pages

En este repositorio me propongo desarrollar, de manera detallada y progresiva, tanto la teoría como algunos elementos prácticos del *record linkage*, tomando como eje tres hitos fundamentales en la literatura:

1. El modelo clásico de comparación de pares de Fellegi-Sunter (1969)
2. El modelo de *matching* con errores de medición en las covariables (*Hit-Miss*) de Copas y Hilton (1990)
3. El modelo de *matching* bipartito propuesto por Sadinle (2014)

Más que un manual exhaustivo o una referencia definitiva, este repositorio nace como un ejercicio personal: un intento de organizar, comprender y explicar estos modelos desde sus fundamentos estadísticos y matemáticos, motivado principalmente por mi curiosidad intelectual. En ese sentido, el énfasis está puesto en descomponer las ideas, hacer explícitas las suposiciones y recorrer paso a paso las estructuras probabilísticas que sustentan cada enfoque.

El hilo conductor de esta exploración es la inferencia bayesiana, que permite unificar de manera natural los distintos modelos bajo un mismo marco conceptual. Por esta razón, además del desarrollo teórico, incluyo implementaciones computacionales que buscan reflejar fielmente las formulaciones probabilísticas. Dichas implementaciones están escritas en STAN, no sólo como herramienta de cálculo, sino también como un lenguaje que ayuda a traducir la teoría en modelos explícitos y reproducibles.

En síntesis, este repositorio debe entenderse como una especie de cuaderno de trabajo extendido: un espacio donde la teoría, la modelación y la implementación dialogan constantemente, con el objetivo principal de profundizar en la comprensión del *record linkage* desde una perspectiva rigurosa pero también exploratoria.

El sitio web de este repositorio es:

<https://psirusteam.github.io/RecordLinkage/>

## Publicación manual en GitHub Pages desde la carpeta principal

Este repositorio ya no necesita GitHub Actions para publicar el sitio. La idea es generar el sitio en su propia PC, subir los archivos resultantes al repositorio y hacer que GitHub Pages sirva directamente la carpeta principal (`/`).

### 1. Desactivar GitHub Actions para el despliegue

1. Elimine o mantenga borrado el archivo `.github/workflows/render-gh-pages.yml`.
2. En GitHub, entre a **Settings > Pages**.
3. En **Build and deployment**, seleccione:
   - **Source:** `Deploy from a branch`
   - **Branch:** `main` (o `master`, según corresponda)
   - **Folder:** `/ (root)`
4. Guarde los cambios.

### 2. Renderizar el sitio localmente

Desde la raíz del repositorio, ejecute:

```bash
Rscript build_site.R
```

Ese script genera en la carpeta principal del repositorio:

- `index.html`, que GitHub Pages publicará como sitio principal.
- `RLteoria.pdf`, que queda disponible para descarga.
- `.nojekyll`, para evitar problemas de publicación con archivos y carpetas del proyecto.

### 3. Subir los archivos generados a GitHub

Después de renderizar, revise los cambios y súbalos al repositorio:

```bash
git status
git add index.html RLteoria.pdf .nojekyll RLteoría.Rmd build_site.R README.md capitulos/ Rcodes/
git commit -m "Update published site"
git push origin main
```

Si sólo cambió el contenido del sitio, puede ajustar la lista de archivos en `git add` según lo que haya modificado realmente.

### 4. Flujo recomendado a partir de ahora

Cada vez que actualice el contenido del sitio:

1. Edite los archivos fuente (`RLteoría.Rmd`, capítulos, scripts, etc.).
2. Ejecute `Rscript build_site.R` en su PC.
3. Revise `index.html` localmente.
4. Haga `git add`, `git commit` y `git push`.
5. Espere a que GitHub Pages vuelva a publicar la carpeta principal.

## Generación del sitio y del PDF

El archivo principal `RLteoría.Rmd` está preparado para generar dos salidas principales:

- `index.html`, que se publica desde la raíz del repositorio.
- `RLteoria.pdf`, un documento PDF descargable directamente desde la propia página web.

Para compilar ambos artefactos, ejecute:

```bash
Rscript build_site.R
```
