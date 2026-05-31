<template>
  <div class="homepage-dashboard">
    <!-- 📊 Premium Stats Highlights -->
    <div class="stats-grid">
      <div class="stat-card">
        <div class="stat-icon">🎓</div>
        <div class="stat-info">
          <div class="stat-value">45+</div>
          <div class="stat-label">Technologies</div>
        </div>
      </div>
      <div class="stat-card">
        <div class="stat-icon">📚</div>
        <div class="stat-info">
          <div class="stat-value">3,500+</div>
          <div class="stat-label">Structured Lessons</div>
        </div>
      </div>
      <div class="stat-card">
        <div class="stat-icon">⚡</div>
        <div class="stat-info">
          <div class="stat-value">Instant</div>
          <div class="stat-label">Offline Search</div>
        </div>
      </div>
    </div>

    <!-- 🔍 Premium Search Input -->
    <div class="search-wrapper">
      <div class="search-input-container">
        <span class="search-icon">🔍</span>
        <input 
          type="text" 
          v-model="search" 
          placeholder="What topic do you want to master today? (e.g., Python, SQL, React)" 
        />
        <span v-if="search" class="clear-search" @click="search = ''">✕</span>
      </div>
    </div>

    <!-- 🏷️ Horizontal Category Filter Tabs -->
    <div class="category-tabs">
      <button 
        v-for="cat in categoryTabs" 
        :key="cat.name"
        :class="['category-tab', { active: selectedCategory === cat.name }]"
        @click="selectedCategory = cat.name"
      >
        <span class="tab-icon">{{ cat.icon }}</span> {{ cat.name }}
      </button>
    </div>

    <!-- 🗂️ Language Categories Grid -->
    <div v-for="(section, idx) in filteredSectionWise" :key="idx" class="section-container">
      <div v-if="section.length">
        <div class="section-header">
          <span class="section-badge">{{ section[0]["type"] }}</span>
          <div class="section-line"></div>
        </div>

        <div class="cards-grid">
          <div 
            v-for="lang in section" 
            :key="lang.id" 
            class="lang-card"
            :style="{ '--accent-color': getCategoryColor(lang.type) }"
          >
            <div class="card-inner">
              <div class="card-top">
                <span class="category-indicator"></span>
                <span class="lessons-badge">{{ lang.topics }} Lessons</span>
              </div>
              
              <h3 class="card-title">{{ lang.name }}</h3>
              
              <a :href="lang.url" class="card-action">
                <span>Start Learning</span>
                <span class="action-arrow">→</span>
              </a>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- 💡 No Results State -->
    <div v-if="isEmpty" class="empty-state">
      <div class="empty-icon">🔎</div>
      <h3>No tutorials found</h3>
      <p>We couldn't find anything matching "{{ search }}". Try searching for common languages like Python, JavaScript, or SQL!</p>
      <button class="reset-button" @click="resetFilters">Reset Search</button>
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      search: "",
      selectedCategory: "All",
      categoryTabs: [
        { name: "All", icon: "🌐" },
        { name: "Essentials", icon: "💡" },
        { name: "Languages", icon: "💻" },
        { name: "Databases", icon: "🗄️" },
        { name: "Web & JS", icon: "⚡" },
        { name: "Terminal", icon: "📟" },
        { name: "Frameworks", icon: "📦" },
        { name: "Mobile", icon: "📱" },
        { name: "Desktop & VBA", icon: "🖥️" },
      ],
      sections: [
        "Essential",
        "Programming Language",
        "Database Technology", 
        "JavaScript Technology", 
        "Terminal",
        "Framework", 
        "Visual Basic", 
        "Mobile Technology",
      ],
      categoryMap: {
        "Essentials": "Essential",
        "Languages": "Programming Language",
        "Databases": "Database Technology",
        "Web & JS": "JavaScript Technology",
        "Terminal": "Terminal",
        "Frameworks": "Framework",
        "Desktop & VBA": "Visual Basic",
        "Mobile": "Mobile Technology"
      },
      languages: [
        { id: 1, name: "Python", topics: 206, url: "/python/", type: "Programming Language" },
        { id: 2, name: "PHP", topics: 120, url: "/php/", type: "Programming Language" },
        { id: 3, name: "JavaScript", topics: 106, url: "/javascript/", type: "Programming Language"},
        { id: 4, name: "Java", topics: 185, url: "/java/", type: "Programming Language" },
        { id: 5, name: "C", topics: 63, url: "/c/", type: "Programming Language" },
        { id: 6, name: "C++", topics: 147, url: "/cpp/", type: "Programming Language" },
        { id: 7, name: "C#", topics: 165, url: "/csharp/", type: "Programming Language" },
        { id: 38, name: "Objective-C", topics: 34, url: "/objectivec/", type: "Programming Language" },
        { id: 14, name: "R", topics: 131, url: "/r/", type: "Programming Language" },
        { id: 22, name: "Haskell", topics: 78, url: "/haskell/", type: "Programming Language" },
        { id: 24, name: "Ruby", topics: 72, url: "/ruby/", type: "Programming Language" },
        { id: 23, name: "Perl", topics: 40, url: "/perl/", type: "Programming Language" },
        { id: 37, name: "MATLAB", topics: 34, url: "/matlab/", type: "Programming Language" },
        { id: 35, name: "LaTex", topics: 17, url: "/latex/", type: "Programming Language" },

        { id: 8, name: "SQL", topics: 64, url: "/sql/", type: "Database Technology" },
        { id: 9, name: "MySQL", topics: 73, url: "/mysql/", type: "Database Technology" },
        { id: 10, name: "MS SQL Server", topics: 113, url: "/mssql/", type: "Database Technology" },
        { id: 11, name: "PostgreSQL", topics: 29, url: "/postgresql/", type: "Database Technology" },
        { id: 26, name: "Oracle Database", topics: 47, url: "/oracle/", type: "Database Technology" },
        { id: 27, name: "MongoDB", topics: 27, url: "/mongodb/", type: "Database Technology" },

        { id: 13, name: "Node.js", topics: 111, url: "/nodejs/", type: "JavaScript Technology" },
        { id: 40, name: "ReactJS", topics: 31, url: "/reactjs/", type: "JavaScript Technology" },
        { id: 33, name: "AngularJS", topics: 52, url: "/angularjs/", type: "JavaScript Technology" },
        { id: 32, name: "Angular 2", topics: 70, url: "/angular2/", type: "JavaScript Technology" },
        { id: 21, name: "TypeScript", topics: 30, url: "/typescript/", type: "JavaScript Technology" },
        { id: 20, name: "jQuery", topics: 18, url: "/jquery/", type: "JavaScript Technology" },

        { id: 15, name: "Algorithm", topics: 65, url: "/algorithm/", type: "Essential" },
        { id: 19, name: "HTML", topics: 59, url: "/html/", type: "Essential" },
        { id: 18, name: "CSS", topics: 56, url: "/css/", type: "Essential" },
        { id: 12, name: "Git", topics: 61, url: "/git/", type: "Essential" },
        
        { id: 36, name: "Linux", topics: 21, url: "/linux/", type: "Terminal" },
        { id: 17, name: "Bash", topics: 71, url: "/bash/", type: "Terminal" },
        { id: 39, name: "PowerShell", topics: 74, url: "/powershell/", type: "Terminal" },
        
        { id: 16, name: ".NET Framework", topics: 59, url: "/dotnet/", type: "Framework" },
        { id: 25, name: "Entity Framework", topics: 23, url: "/entityframework/", type: "Framework" },
        { id: 29, name: "Ruby on Rails", topics: 74, url: "/rubyonrails/", type: "Framework" },
        { id: 30, name: "Spring Framework", topics: 44, url: "/spring/", type: "Framework" },
        { id: 34, name: "iOS", topics: 210, url: "/ios/", type: "Framework" },
        
        { id: 43, name: "Visual Basic .NET", topics: 51, url: "/vbnet/", type: "Visual Basic" },
        { id: 44, name: "VBA", topics: 46, url: "/vba/", type: "Visual Basic" },
        { id: 45, name: "Excel VBA", topics: 31, url: "/excelvba/", type: "Visual Basic" },

        { id: 41, name: "React Native", topics: 32, url: "/reactnative/", type: "Mobile Technology" },
        { id: 31, name: "Android", topics: 268, url: "/android/", type: "Mobile Technology" },
        { id: 42, name: "Swift", topics: 60, url: "/swift/", type: "Mobile Technology" },
        { id: 28, name: "Kotlin", topics: 38, url: "/kotlin/", type: "Mobile Technology" },
        { id: 46, name: "Xamarin", topics: 79, url: "/xamarin/", type: "Mobile Technology" },
      ]
    };
  },
  computed: {
    filteredLanguages() {
      return this.languages.filter(language =>
        language.name.toLowerCase().includes(this.search.toLowerCase())
      );
    },

    filteredSectionWise() {
      return this.sections.map(s => {
        // If specific category selected, check mapping
        if (this.selectedCategory !== "All") {
          const matchedType = this.categoryMap[this.selectedCategory];
          if (s !== matchedType) return [];
        }
        return this.filteredLanguages.filter(l => l.type === s);
      });
    },

    isEmpty() {
      return !this.filteredSectionWise.some(section => section.length > 0);
    }
  },
  methods: {
    getCategoryColor(type) {
      const colors = {
        "Essential": "#10b981", // Emerald
        "Programming Language": "#8b5cf6", // Violet
        "Database Technology": "#06b6d4", // Cyan
        "JavaScript Technology": "#f59e0b", // Amber
        "Terminal": "#6b7280", // Slate
        "Framework": "#ec4899", // Pink
        "Visual Basic": "#3b82f6", // Blue
        "Mobile Technology": "#f43f5e" // Rose
      };
      return colors[type] || "#10b981";
    },
    resetFilters() {
      this.search = "";
      this.selectedCategory = "All";
    }
  }
};
</script>

<style scoped>
.homepage-dashboard {
  max-width: 1240px;
  margin: 0 auto;
  padding: 10px 20px 80px 20px;
  font-family: inherit;
}

/* 📊 Stats Grid Styling */
.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 1.5rem;
  margin-bottom: 3.5rem;
}

.stat-card {
  display: flex;
  align-items: center;
  gap: 1.25rem;
  padding: 1.5rem;
  border-radius: 1rem;
  background: var(--sl-color-bg-subtle, #f3f4f6);
  border: 1px solid var(--sl-color-hairline, #e5e7eb);
  box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);
  transition: transform 0.3s ease, box-shadow 0.3s ease;
}

.stat-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
}

.stat-icon {
  font-size: 2.25rem;
  padding: 0.75rem;
  background: rgba(66, 185, 131, 0.1);
  border-radius: 0.75rem;
}

.stat-value {
  font-size: 1.75rem;
  font-weight: 800;
  color: var(--sl-color-text, #111827);
  line-height: 1.2;
}

.stat-label {
  font-size: 0.875rem;
  color: var(--sl-color-text-muted, #6b7280);
  font-weight: 500;
}

/* 🔍 Search Input Wrapper */
.search-wrapper {
  margin-bottom: 2.5rem;
}

.search-input-container {
  position: relative;
  max-width: 720px;
  margin: 0 auto;
  display: flex;
  align-items: center;
}

.search-icon {
  position: absolute;
  left: 1.25rem;
  font-size: 1.25rem;
  pointer-events: none;
  opacity: 0.7;
}

.search-input-container input {
  width: 100%;
  padding: 1.125rem 3rem 1.125rem 3.25rem;
  font-size: 1.125rem;
  font-weight: 500;
  border-radius: 9999px;
  border: 2px solid var(--sl-color-hairline, #cfd4db);
  background: var(--sl-color-bg, #fff);
  color: var(--sl-color-text, #000);
  outline: none;
  box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.04);
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}

.search-input-container input:focus {
  border-color: #42b983;
  box-shadow: 0 10px 25px -5px rgba(66, 185, 131, 0.25);
  transform: scale(1.01);
}

.clear-search {
  position: absolute;
  right: 1.25rem;
  font-size: 1.125rem;
  cursor: pointer;
  opacity: 0.5;
  transition: opacity 0.2s;
}

.clear-search:hover {
  opacity: 1;
}

/* 🏷️ Category Tabs Switcher */
.category-tabs {
  display: flex;
  gap: 0.75rem;
  overflow-x: auto;
  padding: 0.25rem 0.25rem 1.25rem 0.25rem;
  margin-bottom: 2.5rem;
  scrollbar-width: thin;
  -webkit-overflow-scrolling: touch;
}

.category-tabs::-webkit-scrollbar {
  height: 4px;
}

.category-tabs::-webkit-scrollbar-thumb {
  background: var(--sl-color-hairline, #cfd4db);
  border-radius: 4px;
}

.category-tab {
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.625rem 1.25rem;
  border-radius: 9999px;
  font-size: 0.9375rem;
  font-weight: 600;
  background: var(--sl-color-bg-subtle, #f3f4f6);
  border: 1px solid var(--sl-color-hairline, #e5e7eb);
  color: var(--sl-color-text-muted, #4b5563);
  cursor: pointer;
  transition: all 0.25s ease;
}

.category-tab:hover {
  background: var(--sl-color-hairline, #e5e7eb);
  color: var(--sl-color-text, #111827);
  transform: translateY(-1px);
}

.category-tab.active {
  background: #42b983;
  color: #fff;
  border-color: #42b983;
  box-shadow: 0 4px 12px rgba(66, 185, 131, 0.35);
}

.tab-icon {
  font-size: 1.05rem;
}

/* 🗂️ Language Categories Sections */
.section-container {
  margin-bottom: 3.5rem;
}

.section-header {
  display: flex;
  align-items: center;
  gap: 1rem;
  margin-bottom: 1.75rem;
}

.section-badge {
  font-size: 1rem;
  font-weight: 800;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  color: var(--sl-color-text, #1f2937);
  background: rgba(66, 185, 131, 0.08);
  border: 1px solid rgba(66, 185, 131, 0.25);
  padding: 0.375rem 0.875rem;
  border-radius: 0.5rem;
}

.section-line {
  flex-grow: 1;
  height: 2px;
  background: linear-gradient(90deg, rgba(66, 185, 131, 0.25) 0%, transparent 100%);
}

/* 🎴 Cards Grid and 3D Hover Card Styles */
.cards-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1.5rem;
}

.lang-card {
  position: relative;
  border-radius: 1rem;
  background: var(--sl-color-bg-subtle, #ffffff);
  border: 1px solid var(--sl-color-hairline, #e5e7eb);
  box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.03);
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  overflow: hidden;
}

.lang-card::before {
  content: "";
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 4px;
  background: var(--accent-color, #42b983);
}

.card-inner {
  padding: 1.5rem;
  display: flex;
  flex-direction: column;
  height: 100%;
}

.card-top {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 1.25rem;
}

.category-indicator {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  background: var(--accent-color, #42b983);
}

.lessons-badge {
  font-size: 0.8rem;
  font-weight: 700;
  color: var(--accent-color, #42b983);
  background: rgba(66, 185, 131, 0.08);
  padding: 0.25rem 0.625rem;
  border-radius: 9999px;
  transition: all 0.3s ease;
}

.card-title {
  font-size: 1.625rem;
  font-weight: 800;
  color: var(--sl-color-text, #111827);
  margin: 0 0 1.5rem 0;
  line-height: 1.2;
}

.card-action {
  margin-top: auto;
  display: inline-flex;
  align-items: center;
  justify-content: space-between;
  text-decoration: none;
  font-size: 0.9375rem;
  font-weight: 700;
  color: var(--accent-color, #42b983);
  padding-top: 0.75rem;
  border-top: 1px dashed var(--sl-color-hairline, #f3f4f6);
  transition: color 0.3s ease;
}

.action-arrow {
  transition: transform 0.3s ease;
}

/* Hover Animations */
.lang-card:hover {
  transform: translateY(-6px);
  box-shadow: 0 12px 25px -4px rgba(0, 0, 0, 0.08);
  border-color: var(--accent-color, #42b983);
}

.lang-card:hover .lessons-badge {
  background: var(--accent-color, #42b983);
  color: #fff;
}

.lang-card:hover .action-arrow {
  transform: translateX(6px);
}

/* 💡 Empty Search State Styling */
.empty-state {
  text-align: center;
  padding: 5rem 2rem;
  background: var(--sl-color-bg-subtle, #f9fafb);
  border: 1px dashed var(--sl-color-hairline, #e5e7eb);
  border-radius: 1.5rem;
}

.empty-icon {
  font-size: 3.5rem;
  margin-bottom: 1.25rem;
}

.empty-state h3 {
  font-size: 1.5rem;
  font-weight: 800;
  color: var(--sl-color-text, #111827);
  margin-bottom: 0.5rem;
}

.empty-state p {
  font-size: 1rem;
  color: var(--sl-color-text-muted, #6b7280);
  max-width: 480px;
  margin: 0 auto 1.5rem auto;
}

.reset-button {
  padding: 0.75rem 1.5rem;
  font-size: 0.9375rem;
  font-weight: 700;
  background: #42b983;
  color: #fff;
  border: none;
  border-radius: 9999px;
  cursor: pointer;
  box-shadow: 0 4px 12px rgba(66, 185, 131, 0.35);
  transition: all 0.2s ease;
}

.reset-button:hover {
  transform: translateY(-1px);
  box-shadow: 0 6px 16px rgba(66, 185, 131, 0.45);
}
</style>