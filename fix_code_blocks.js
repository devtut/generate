const fs = require('fs');

const files = [
  'docs/mongodb/getting-started-with-mongodb.md',
  'docs/android/getting-started-with-android.md',
  'docs/css/backgrounds.md',
  'docs/xamarin/xamarin-ios-navigation-drawer.md',
  'docs/bash/getting-started-with-bash.md',
  'docs/html/marking-up-computer-code.md'
];

files.forEach(file => {
  if (fs.existsSync(file)) {
    let content = fs.readFileSync(file, 'utf8');

    // Replace opening tags
    // <pre class="lang-java prettyprint-override"><code> -> ```java
    // <pre class="lang-xml prettyprint-override"><code> -> ```xml
    // escape special regex chars in search if needed, but here simple replacement loops work

    content = content.replace(/<pre class="lang-([a-zA-Z0-9]+) prettyprint-override"><code>/g, (match, lang) => {
      return "```" + lang + "\n";
    });

    // Also check if there are closing </code></pre> tags that need to be ```
    // In the android example, the closing was ``` already. But maybe in others it's </code></pre>
    content = content.replace(/<\/code><\/pre>/g, "```\n");

    fs.writeFileSync(file, content);
    console.log(`Fixed ${file}`);
  }
});

// Fix the python file
const pyFile = 'docs/python/dynamic-code-execution-with-exec-and-eval.md';
if (fs.existsSync(pyFile)) {
  let content = fs.readFileSync(pyFile, 'utf8');
  if (!content.trim().endsWith('</s>')) {
    content += '\n</s>\n';
    fs.writeFileSync(pyFile, content);
    console.log(`Fixed ${pyFile}`);
  }
}
