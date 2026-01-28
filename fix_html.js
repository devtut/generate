const fs = require('fs');
const path = require('path');

const allowedTags = [
  'kbd', 'br', 'hr', 'img', 'a', 'b', 'strong', 'i', 'em', 'u', 's', 'del',
  'code', 'pre', 'sub', 'sup', 'details', 'summary', 'blockquote', 'p'
];

function escapeTagsInText(text) {
  // Escape opening tags
  let newText = text.replace(/<([a-zA-Z0-9]+)([^>]*)>/g, (match, tag, attrs) => {
    if (allowedTags.includes(tag.toLowerCase())) {
      return match;
    }
    return `&lt;${tag}${attrs}>`;
  });

  // Escape closing tags
  newText = newText.replace(/<\/([a-zA-Z][a-zA-Z0-9]*)\s*>/g, (match, tag) => {
    if (allowedTags.includes(tag.toLowerCase())) {
      return match;
    }
    return `&lt;/${tag}>`;
  });

  return newText;
}

function processFile(filePath) {
  let content = fs.readFileSync(filePath, 'utf8');

  // Split by code blocks to avoid changing code
  // Matches: 
  // 1. ``` ... ``` (fenced)
  // 2. ` ... ` (inline, simplified, might miss edge cases but good enough)
  // Note: capturing group in split includes the separator in the result
  const parts = content.split(/(```[\s\S]*?```|`[^`\n]+`)/g);

  let modified = false;
  const newParts = parts.map(part => {
    if (part.startsWith('```') || (part.startsWith('`') && part.endsWith('`'))) {
      return part;
    }
    const escaped = escapeTagsInText(part);
    if (escaped !== part) modified = true;
    return escaped;
  });

  if (modified) {
    console.log(`Fixing ${filePath}`);
    fs.writeFileSync(filePath, newParts.join(''));
  }
}

function getAllFiles(dirPath, arrayOfFiles) {
  const files = fs.readdirSync(dirPath)
  arrayOfFiles = arrayOfFiles || []
  files.forEach(function (file) {
    if (fs.statSync(dirPath + "/" + file).isDirectory()) {
      arrayOfFiles = getAllFiles(dirPath + "/" + file, arrayOfFiles)
    } else {
      if (file.endsWith('.md')) {
        arrayOfFiles.push(path.join(dirPath, "/", file))
      }
    }
  })
  return arrayOfFiles
}

const files = getAllFiles('./docs');
files.forEach(f => processFile(f));
