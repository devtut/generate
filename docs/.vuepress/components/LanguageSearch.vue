<template>
  <div>
    <div class="search">
      <input type="text" v-model="search" placeholder="Seach a topic" />
    </div>

    <div v-for="(section, index) in filteredSectionWise" :key="index">
      <div v-if="section.length">
        <div class="section-title"><span class="design-arrow"><<<</span> {{ section[0]["type"] }} <span class="design-arrow">>>></span></div>
        <div class="cards">
          <div class="card" v-for="language in section" :key="language.id">
            <div class="title">{{ language.name }}</div>
            <router-link :to="{ path: `${language.url}` }">
              <div class="topics">Master {{ language.topics }} lessons →</div>
            </router-link>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      search: "",
      sections: [
        "Essential",
        "Programming Language",
        "Database Technology", 
        "JavaScript Technology", 
        "Mobile Technology",
        "Framework", 
        "Visual Basic", 
      ],
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
          return this.filteredLanguages.filter(l => l.type === s)
      })
    }
  }
};
</script>

<style scoped>
.search {
  text-align: center;
  padding-bottom: 50px;
}

.search input {
  cursor: text;
  width: 15rem;
  height: 2.5rem;
  color: #4e6e8e;
  display: inline-block;
  border: 3px solid #cfd4db;
  border-radius: 2rem;
  font-size: 1.1rem;
  line-height: 2.5rem;
  padding: 0 0.5rem 0 2.5rem;
  outline: none;
  background: #fff url(/assets/img/search.83621669.svg) 0.6rem 0.5rem no-repeat;
  background-size: 1.5rem;
}

.search input:focus {
  border-color: #42b983;
}

.design-arrow {
  color: #42b983;
}

@media (max-width: 719px) {
  .design-arrow {
    display: none;
  }
}

.section-title {
  font-size: 28px;
  font-weight: bold;
  text-align: center;
  color: #25434D;
}

.cards {
  padding: 20px 0 50px 0;
  max-width: 1200px;
  margin: 0 auto;
  display: grid;
  grid-gap: 1.5rem 1rem;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
}

.title {
  font-size: 34px;
  font-weight: bold;
  border: 2px solid #42b983;
  height: 3rem;
  padding: 1rem;
  color: #42b983;
  text-align: center;
}

.topics {
  font-size: 16px;
  font-weight: bold;
  color: white;
  background-color: #42b983;
  padding: 0.5rem;
  text-align: center;
  text-transform: uppercase;
}
</style>