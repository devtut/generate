const term = 'In JDK <7u6 versions the `substring` method';

const allowedTags = [
  'kbd', 'br', 'hr', 'img', 'a', 'b', 'strong', 'i', 'em', 'u', 's', 'del',
  'code', 'pre', 'sub', 'sup', 'details', 'summary', 'blockquote', 'p'
];

function escapeTagsInText(text) {
  // Escape opening tags
  let newText = text.replace(/<([a-zA-Z0-9]+)/g, (match, tag) => {
    console.log(`Matched: ${match} with tag: ${tag}`);
    if (allowedTags.includes(tag.toLowerCase())) {
      return match;
    }
    return `&lt;${tag}`;
  });
  return newText;
}

console.log(escapeTagsInText(term));
