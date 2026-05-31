const fs = require('fs');
const path = require('path');

const srcDocsDir = path.join(__dirname, 'src', 'content', 'docs');
const backupDir = path.join(__dirname, 'temp-backup-docs');

// Create backup dir
if (!fs.existsSync(backupDir)) {
  fs.mkdirSync(backupDir, { recursive: true });
}

console.log('⚡ Starting single-module isolation for JavaScript...');

// 1. Move all directories except 'javascript'
const items = fs.readdirSync(srcDocsDir);
for (const item of items) {
  const itemPath = path.join(srcDocsDir, item);
  const stat = fs.statSync(itemPath);

  if (stat.isDirectory()) {
    if (item !== 'javascript') {
      const destPath = path.join(backupDir, item);
      if (fs.existsSync(destPath)) {
        fs.rmSync(destPath, { recursive: true, force: true });
      }
      console.log(`📦 Moving ${item} to temp backup...`);
      fs.renameSync(itemPath, destPath);
    }
  }
}

// 2. Filter sidebars.json
const sidebarsPath = path.join(__dirname, 'sidebars.json');
if (fs.existsSync(sidebarsPath)) {
  const sidebars = JSON.parse(fs.readFileSync(sidebarsPath, 'utf8'));
  const jsSidebar = sidebars.filter(group => group.label === 'JavaScript');
  
  // Backup full sidebar
  fs.writeFileSync(path.join(__dirname, 'sidebars.full.json'), JSON.stringify(sidebars, null, 2), 'utf8');
  // Write filtered sidebar
  fs.writeFileSync(sidebarsPath, JSON.stringify(jsSidebar, null, 2), 'utf8');
  console.log('⚡ Filtered sidebars.json to only include JavaScript!');
}

console.log('🎉 Single-module isolation complete! Ready to run yarn dev.');
