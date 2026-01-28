const fs = require('fs');
const path = require('path');

const allowedTags = [
  'kbd', 'br', 'hr', 'img', 'a', 'b', 'strong', 'i', 'em', 'u', 's', 'del',
  'code', 'pre', 'sub', 'sup', 'details', 'summary', 'blockquote', 'p', 'div', 'span', 'ul', 'ol', 'li', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
  'table', 'thead', 'tbody', 'tr', 'th', 'td', 'dl', 'dt', 'dd'
];

const voidTags = [
  'br', 'hr', 'img', 'input', 'meta', 'link'
];

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

files.forEach(file => {
  const content = fs.readFileSync(file, 'utf8');

  // Create a version of content with code blocks masked (replaced by spaces)
  // to avoid false positives inside code blocks, but preserve line numbers.
  // Masking with spaces keeps indices same.

  let cleanContent = content;

  // Mask ``` blocks
  cleanContent = cleanContent.replace(/```[\s\S]*?```/g, (match) => {
    return match.replace(/[^\n]/g, ' ');
  });

  // Mask inline code `...`
  cleanContent = cleanContent.replace(/`[^`]*`/g, (match) => {
    return ' '.repeat(match.length);
  });

  const stack = [];

  // Regex to find tags
  const tagRegex = /<\/?([a-zA-Z0-9]+)[^>]*>/g;
  let match;

  while ((match = tagRegex.exec(cleanContent)) !== null) {
    const tagFull = match[0];
    const tagName = match[1].toLowerCase();
    const isClosing = tagFull.startsWith('</');
    const isSelfClosing = tagFull.endsWith('/>') || voidTags.includes(tagName);

    const index = match.index;
    const lineNumber = cleanContent.substring(0, index).split('\n').length;

    // Filter logic:
    // If tag is allowed, we check nesting.
    // If tag is NOT allowed, we report it as "Unknown/Invalid" immediately unless it's obviously not a tag?
    // For this task, we want to find "Unclosed" allowed tags AND "Unknown" tags.

    if (!allowedTags.includes(tagName)) {
      // It's likely a typo or text like <int>
      // Report it.
      console.log(`Unknown tag <${tagName}> in ${file}:${lineNumber}`);
    } else {
      // Allowed tag logic
      if (isClosing) {
        // Try to pop
        let found = false;
        for (let i = stack.length - 1; i >= 0; i--) {
          if (stack[i].tag === tagName) {
            stack.splice(i, 1); // Remove matched and everything after it (if we were strict)
            // But since HTML is loose, maybe we just remove this one.
            // Stack: [p, b]. Closing </b> -> removes b. Stack: [p].
            // Stack: [p, b]. Closing </p> -> removes p? Warns about b?
            // Let's simple pop matching.
            found = true;
            // Remove the match from stack
            stack.splice(i, 1);
            // In strictly valid HTML, we should complain about tags in between.
            // But here we just want to find UNMATCHED openers.
            break;
          }
        }
        if (!found) {
          // Extra closing tag
          // console.log(`Extra closing tag </${tagName}> in ${file}:${lineNumber}`); 
        }
      } else if (!isSelfClosing) {
        stack.push({ tag: tagName, line: lineNumber });
      }
    }
  }

  // Report unclosed tags
  if (stack.length > 0) {
    console.log(`Unclosed tags in ${file}:`);
    stack.forEach(s => console.log(`  <${s.tag}> opened at line ${s.line}`));
  }
});
