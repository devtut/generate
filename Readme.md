# 🚀 DevTut — Modern Programming Tutorials

DevTut is a modern, high-performance reference portal featuring example-based programming tutorials for solid developers. It covers **45+ programming topics** and **3,500+ tutorials** compiled for speed and modern search engine optimization (SEO).

This portal has been fully migrated from a legacy VuePress architecture to a state-of-the-art **Astro 6** and **Starlight** documentation engine.

---

## 🛠️ Technology Stack

* **Core Documentation Engine:** [Astro v6 (ESM)](https://astro.build/)
* **Theme & Navigation:** [Starlight v0.39](https://starlight.astro.build/)
* **Client-Side Framework:** [Vue 3.5](https://vuejs.org/) (for interactive search widgets)
* **Search Integration:** [Pagefind](https://pagefind.app/) (high-performance serverless search indexer)
* **Styling & Theming:** Custom Emerald Green Design System (`#42b983`)

---

## 💻 Local Development

### 1. Installation
Install project dependencies using Yarn:
```bash
yarn install
```

### 2. Run Development Server
Start the local development server (page compiles on demand for instant startup):
```bash
yarn dev
```
Open **`http://localhost:4321/`** in your browser.

---

## ⚡ Development Isolation (Blazing Fast)

Because this repository contains over 3,500 pages, compiling all languages can take longer during active development. You can isolate your workspace to focus only on a single language (e.g., **JavaScript**):

* **Switch to Isolated Mode (JavaScript only):**
  ```bash
  node migrate.cjs && node isolate-javascript.cjs
  ```
  *(Then run `yarn dev` for sub-second, instant hot reloads!)*

* **Switch back to Full Repository Mode:**
  ```bash
  node migrate.cjs
  ```

---

## 🏗️ Production Build & Deployment

### 1. Compile static HTML bundle
Build highly optimized static assets (compiles static pages, Pagefind search indexes, and SEO sitemaps):
```bash
yarn docs:build
```

### 2. Deploy to GitHub Pages
To build and publish the latest version of the site directly to GitHub Pages, run the automated script:
```bash
./deploy.sh
```

---
*MIT Licensed | Copyright © 2020-present DevTut*
