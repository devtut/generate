const fs = require('fs');
const path = require('path');

// 1. Require the old VuePress configuration
console.log('⚡ Loading old VuePress configuration...');
const oldConfig = require('./docs/.vuepress/config.cjs');
const sidebar = oldConfig.themeConfig.sidebar;

// 2. Define directory paths
const srcDocsDir = path.join(__dirname, 'src', 'content', 'docs');
const srcComponentsDir = path.join(__dirname, 'src', 'components');
const publicDir = path.join(__dirname, 'public');

console.log('⚡ Creating Astro Starlight directories...');
fs.mkdirSync(srcDocsDir, { recursive: true });
fs.mkdirSync(srcComponentsDir, { recursive: true });
fs.mkdirSync(publicDir, { recursive: true });

// Helper to escape string for frontmatter
function escapeFrontmatterString(str) {
  if (!str) return '';
  return str.replace(/"/g, '\\"');
}

// 3. Process each sidebar / language directory
const starlightSidebar = [];

for (const key of Object.keys(sidebar)) {
  const languagePath = key.replace(/^\/|\/$/g, ''); // E.g., 'sql', 'typescript'
  if (!languagePath) continue;

  const configGroup = sidebar[key][0];
  const title = configGroup.title;
  const children = configGroup.children;

  console.log(`\n📦 Processing language: ${title} (${languagePath})...`);

  const starlightGroup = {
    label: title,
    items: []
  };

  const oldLangDir = path.join(__dirname, 'docs', languagePath);
  const newLangDir = path.join(srcDocsDir, languagePath);

  if (!fs.existsSync(oldLangDir)) {
    console.log(`⚠️ Warning: Directory docs/${languagePath} does not exist. Skipping file migration for this topic.`);
    continue;
  }

  // Create new directory in src/content/docs
  fs.mkdirSync(newLangDir, { recursive: true });

  // Move files and process frontmatter
  const titleMap = {};
  const files = fs.readdirSync(oldLangDir);
  for (const file of files) {
    const filePath = path.join(oldLangDir, file);
    const stat = fs.statSync(filePath);

    if (stat.isDirectory()) continue;

    let targetFileName = file;
    if (file.toLowerCase() === 'readme.md') {
      targetFileName = 'index.md';
    }

    const newFilePath = path.join(newLangDir, targetFileName);

    // Read and process markdown file
    let content = fs.readFileSync(filePath, 'utf8');

    // Parse existing frontmatter and title
    let titleVal = '';
    let descriptionVal = '';

    // Check if the file has frontmatter
    const frontmatterRegex = /^---([\s\S]*?)---/;
    const frontmatterMatch = content.match(frontmatterRegex);

    let frontmatterContent = '';
    let bodyContent = content;

    if (frontmatterMatch) {
      frontmatterContent = frontmatterMatch[1];
      bodyContent = content.replace(frontmatterRegex, '').trim();

      // Extract existing metaTitle or title
      const metaTitleMatch = frontmatterContent.match(/metaTitle:\s*"([^"]+)"/);
      if (metaTitleMatch) {
        titleVal = metaTitleMatch[1];
        // Clean up leading language prefix in metaTitle (e.g. "SQL - Select" -> "Select")
        if (titleVal.includes(' - ')) {
          titleVal = titleVal.split(' - ').slice(1).join(' - ');
        }
      }

      const descMatch = frontmatterContent.match(/description:\s*"([^"]+)"/);
      if (descMatch) {
        descriptionVal = descMatch[1];
      }
    }

    // If we didn't find a title in frontmatter, look for the first # H1 heading
    if (!titleVal) {
      const h1Match = bodyContent.match(/^#\s+(.+)$/m);
      if (h1Match) {
        titleVal = h1Match[1].trim();
      }
    }

    // Fallback if no title is found at all
    if (!titleVal) {
      titleVal = targetFileName.replace(/\.md$/, '').replace(/-/g, ' ');
      // Capitalize
      titleVal = titleVal.charAt(0).toUpperCase() + titleVal.slice(1);
    }

    // Store in lookup map for sidebar labels
    const fileBase = file.replace(/\.md$/, '');
    titleMap[fileBase] = titleVal;

    // Rebuild the frontmatter for Astro Starlight
    const cleanTitle = escapeFrontmatterString(titleVal);
    const cleanDesc = escapeFrontmatterString(descriptionVal || `Tutorial about ${titleVal}`);

    const newFrontmatter = `---
title: "${cleanTitle}"
description: "${cleanDesc}"
---

`;

    // Ensure we don't keep duplicate high-level H1 title that Starlight renders automatically
    // (Starlight renders the 'title' frontmatter as the main H1, so we should strip the first H1 in body if it exists)
    let cleanBody = bodyContent;
    const h1Regex = /^#\s+.+$/m;
    if (h1Regex.test(cleanBody)) {
      cleanBody = cleanBody.replace(h1Regex, '').trim();
    }

    // Convert VuePress ::: container syntax to standard Markdown blockquotes
    cleanBody = cleanBody.replace(/:::\s*(\w+)([^\n]*)\n([\s\S]*?)\n:::/g, (match, type, title, content) => {
      const capType = type.charAt(0).toUpperCase() + type.slice(1);
      const header = title.trim() ? title.trim() : capType;
      return `> **${header}:**\n` + content.split('\n').map(line => `> ${line}`).join('\n');
    });

    fs.writeFileSync(newFilePath, newFrontmatter + cleanBody, 'utf8');
  }

  // Construct Starlight sidebar items based on the old config order
  for (const child of children) {
    if (Array.isArray(child)) {
      // It's like ["", "Disclaimer"]
      const link = `/${languagePath}/`;
      starlightGroup.items.push({ label: child[1], link: link });
    } else {
      // It's a string representing a file
      const fileBase = child;
      const link = `/${languagePath}/${fileBase}/`;
      const resolvedTitle = titleMap[fileBase] || fileBase.replace(/-/g, ' ').replace(/\b\w/g, c => c.toUpperCase());
      starlightGroup.items.push({ label: resolvedTitle, link: link });
    }
  }

  starlightSidebar.push(starlightGroup);
}

// 4. Save the sidebars configuration as a JSON file
console.log('\n💾 Saving Starlight sidebar configuration...');
fs.writeFileSync(
  path.join(__dirname, 'sidebars.json'),
  JSON.stringify(starlightSidebar, null, 2),
  'utf8'
);

// 5. Copy public assets
console.log('⚡ Copying public assets...');
const oldPublicDir = path.join(__dirname, 'docs', '.vuepress', 'public');
if (fs.existsSync(oldPublicDir)) {
  const publicFiles = fs.readdirSync(oldPublicDir);
  for (const file of publicFiles) {
    fs.copyFileSync(
      path.join(oldPublicDir, file),
      path.join(publicDir, file)
    );
  }
}

// 5.5 Copy logo to src/assets for Astro Starlight processing
console.log('⚡ Processing logo asset...');
const srcAssetsDir = path.join(__dirname, 'src', 'assets');
fs.mkdirSync(srcAssetsDir, { recursive: true });
if (fs.existsSync(path.join(publicDir, 'logo.png'))) {
  fs.copyFileSync(
    path.join(publicDir, 'logo.png'),
    path.join(srcAssetsDir, 'logo.png')
  );
}

// 6. Migrate and convert LanguageSearch.vue to Vue 3 compatible syntax
console.log('⚡ Migrating LanguageSearch.vue...');
const oldSearchComponent = path.join(__dirname, 'docs', '.vuepress', 'components', 'LanguageSearch.vue');
const newSearchComponent = path.join(srcComponentsDir, 'LanguageSearch.vue');

if (fs.existsSync(oldSearchComponent)) {
  let searchContent = fs.readFileSync(oldSearchComponent, 'utf8');
  // Replace <router-link> with <a> tag
  searchContent = searchContent.replace(/<router-link\s+([^>]*?)to="\{ path: `\$\{language.url\}` \}">/g, '<a :href="language.url">');
  searchContent = searchContent.replace(/<\/router-link>/g, '</a>');
  // Replace background-size and absolute paths for images if needed
  searchContent = searchContent.replace(/\/assets\/img\/search\.83621669\.svg/g, '/search.svg');
  fs.writeFileSync(newSearchComponent, searchContent, 'utf8');
}

// Create a mock search.svg if it doesn't exist
const searchSvgPath = path.join(publicDir, 'search.svg');
if (!fs.existsSync(searchSvgPath)) {
  fs.writeFileSync(
    searchSvgPath,
    `<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="2" stroke="#cfd4db" width="24" height="24">
      <path stroke-linecap="round" stroke-linejoin="round" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
    </svg>`,
    'utf8'
  );
}

// 7. Write the homepage
console.log('⚡ Writing Astro homepage...');
const newHomepagePath = path.join(srcDocsDir, 'index.mdx');
const homepageContent = `---
title: DevTut
description: Example based programming tutorials for solid developers. Master 45+ programming topics.
template: splash
hero:
  title: DevTut
  tagline: Example based programming tutorials for solid developers. Master 45+ programming topics.
  image:
    file: ../../assets/logo.png
---

import LanguageSearch from '../../components/LanguageSearch.vue';

<LanguageSearch client:load />
`;
fs.writeFileSync(newHomepagePath, homepageContent, 'utf8');

// 8. Generate TSConfig
console.log('⚡ Creating modern tsconfig.json...');
const tsConfig = {
  "extends": "astro/tsconfigs/strict",
  "compilerOptions": {
    "jsx": "preserve",
    "jsxImportSource": "vue"
  }
};
fs.writeFileSync(
  path.join(__dirname, 'tsconfig.json'),
  JSON.stringify(tsConfig, null, 2),
  'utf8'
);

console.log('🎉 Starlight migration script complete!');
console.log('To complete migration:');
console.log('1. Run the script: node migrate.cjs');
console.log('2. Replace package.json with Astro Starlight version.');
console.log('3. Run yarn install');
console.log('4. Run yarn dev');
