# Claude Notes for blog-reboot

## Pkgdown Sites (ggfunction, ggvfields) — CSS/JS Fix

The pkgdown documentation sites are hosted as static HTML dropped into `public/ggfunction/` and `public/ggvfields/`. They are NOT built by Hugo — the Netlify build command is `""` (empty), so only `public/` is served directly.

### The trailing-slash problem

Netlify serves `public/ggfunction/index.html` at both `/ggfunction` and `/ggfunction/`. When the browser loads the page at `/ggfunction` (no trailing slash), **all relative paths break** — `deps/bootstrap.min.css` resolves to `/deps/bootstrap.min.css` (404) instead of `/ggfunction/deps/bootstrap.min.css`.

**Symptoms:** Page looks like unstyled raw HTML. DevTools Console shows 404 errors for jquery, bootstrap.min.css, pkgdown.js, etc. curl will show everything working fine because it doesn't resolve relative paths.

### The fix: `<base href>` tags

Every HTML file needs a `<base href="/ggfunction/">` (or `/ggvfields/`) tag injected after `<head>`, with the href matching the file's directory path. For example:
- `public/ggfunction/index.html` → `<base href="/ggfunction/">`
- `public/ggfunction/reference/index.html` → `<base href="/ggfunction/reference/">`

The GitHub Action workflows in each package repo (`ggfunction/.github/workflows/move-docs.yml` and `ggvfields/.github/workflows/move-file.yml`) have a step that does this automatically after rsync. If the base tags are missing, run:

```bash
for f in $(find public/ggfunction -name "*.html"); do
  dir=$(dirname "$f" | sed 's|^public/||')
  sed -i "s|<head>|<head><base href=\"/$dir/\">|" "$f"
done
```

### Things that DON'T fix it

- `skip_processing = true` in netlify.toml — Netlify wasn't modifying the CSS
- `_redirects` or `[[redirects]]` with trailing slash — causes redirect loops on Netlify
- SRI integrity attribute removal — pkgdown's local CSS doesn't use SRI attributes

### Architecture

- `netlify.toml`: `command = ""`, `publish = "public"` — no Hugo build on Netlify
- `layouts/partials/head.html`: Custom override that removes SRI integrity attributes from Hugo-generated blog pages (separate issue from pkgdown)
- Pkgdown docs are deployed via GitHub Actions that rsync `docs/` → `public/<package>/` in this repo
