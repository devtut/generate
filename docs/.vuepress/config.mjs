import { defineUserConfig } from 'vuepress'
import { defaultTheme } from '@vuepress/theme-default'
import { backToTopPlugin } from '@vuepress/plugin-back-to-top'
import { googleAnalyticsPlugin } from '@vuepress/plugin-google-analytics'
import { seoPlugin } from 'vuepress-plugin-seo2'
import { sitemapPlugin } from 'vuepress-plugin-sitemap2'
import { docsearchPlugin } from '@vuepress/plugin-docsearch'
import { viteBundler } from '@vuepress/bundler-vite'
import sidebar from './sidebar.mjs'
import { registerComponentsPlugin } from '@vuepress/plugin-register-components'
import { getDirname, path } from '@vuepress/utils'

const __dirname = getDirname(import.meta.url)

export default defineUserConfig({
  bundler: viteBundler(),
  // ... (keep existing title/desc/head)
  title: "DevTut",
  description: "Example based programming tutorials for solid developers. Master 45+ programming topics.",
  head: [
    ["link", { rel: "icon", type: "image/png", sizes: "32x32", href: "/favicon-32x32.png" }],
    ["link", { rel: "icon", type: "image/png", sizes: "16x16", href: "/favicon-16x16.png" }],
    ["link", { rel: "manifest", href: "/site.webmanifest" }],
    ["meta", { name: "theme-color", content: "#ffffff" }],
    ["meta", { name: "apple-mobile-web-app-capable", content: "yes" }],
    ["meta", { name: "apple-mobile-web-app-status-bar-style", content: "black" }],
    ["link", { rel: "apple-touch-icon", sizes: "180x180", href: "/apple-touch-icon.png" }],
    ["link", { rel: "mask-icon", href: "/safari-pinned-tab.svg", color: "#5bbad5" }],
    ["meta", { name: "msapplication-TileImage", content: "/mstile-150x150.png" }],
    ["meta", { name: "msapplication-TileColor", content: "#da532c" }],
    ["meta", { name: "google-site-verification", content: "76_rKXgwMVIjd-axJC_1zPV9OS4mEjvtgjYOWVkAdnQ" }],
  ],
  shouldPrefetch: false,
  theme: defaultTheme({
    logo: "/logo.png",
    repo: "devtut/generate",
    editLink: true,
    editLinkText: "Edit this page on GitHub",
    lastUpdated: false,
    docsDir: "docs",
    sidebar: sidebar,
  }),
  plugins: [
    registerComponentsPlugin({
      componentsDir: path.resolve(__dirname, './components'),
    }),
    backToTopPlugin(),
    googleAnalyticsPlugin({
      id: "UA-145300963-3",
    }),
    sitemapPlugin({
      hostname: "https://devtut.github.io",
    }),
    seoPlugin({
      hostname: "https://devtut.github.io",
      author: "DevTut",
    }),
    docsearchPlugin({
      apiKey: "028db77cb984cc13fd130716ea60a98c",
      indexName: "devtut",
      appId: "BH4D9OD16A",
    }),
  ],
})
