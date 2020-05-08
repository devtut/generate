<template>
  <div>
    <div class="search">
      <input type="text" v-model="search" placeholder="Seach topic" />
    </div>

    <div class="cards">
      <div class="card" v-for="language in filteredLanguages" :key="language.id">
        <div class="title">{{ language.name }}</div>
        <router-link :to="{ path: `${language.url}` }">
          <div class="topics">Master {{ language.topics }} topics →</div>
        </router-link>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      search: "",
      languages: [
        { id: 1, name: "Python", topics: 206, url: "/python/" },
        { id: 2, name: "PHP", topics: 120, url: "/php/" },
        { id: 3, name: "JavaScript", topics: 106, url: "/javascript/" },
        { id: 4, name: "Java", topics: 185, url: "/java/" },
        { id: 5, name: "C", topics: 63, url: "/c/" },
        { id: 6, name: "C++", topics: 147, url: "/cpp/" },
        { id: 7, name: "C#", topics: 165, url: "/csharp/" },
        { id: 8, name: "SQL", topics: 64, url: "/sql/" },
        { id: 9, name: "MySQL", topics: 73, url: "/mysql/" },
        { id: 10, name: "MS SQL Server", topics: 113, url: "/mssql/" },
        { id: 11, name: "PostgreSQL", topics: 29, url: "/postgresql/" },
        { id: 12, name: "Git", topics: 61, url: "/git/" },
        { id: 13, name: "Node.js", topics: 111, url: "/nodejs/" },
        { id: 14, name: "R", topics: 131, url: "/r/" },
        { id: 15, name: "Algorithm", topics: 65, url: "/algorithm/" },
        { id: 16, name: ".NET Framework", topics: 59, url: "/dotnet/" },
        { id: 17, name: "Bash", topics: 71, url: "/bash/" },
        { id: 18, name: "CSS", topics: 56, url: "/css/" },
        { id: 19, name: "HTML", topics: 59, url: "/html/" },
        { id: 20, name: "jQuery", topics: 18, url: "/jquery/" },
        { id: 21, name: "TypeScript", topics: 30, url: "/typescript/" },
        { id: 22, name: "Haskell", topics: 78, url: "/haskell/" },
        { id: 23, name: "Perl", topics: 40, url: "/perl/" },
        { id: 24, name: "Ruby", topics: 72, url: "/ruby/" },
        { id: 25, name: "Entity Framework", topics: 23, url: "/entityframework/" },
        { id: 26, name: "Oracle Database", topics: 47, url: "/oracle/" },
        { id: 27, name: "MongoDB", topics: 27, url: "/mongodb/" },
        { id: 28, name: "Kotlin", topics: 38, url: "/kotlin/" },
        { id: 29, name: "Ruby on Rails", topics: 74, url: "/rubyonrails/" },
        { id: 30, name: "Spring Framework", topics: 44, url: "/spring/" },
        { id: 31, name: "Android", topics: 268, url: "/android/" },
        { id: 32, name: "Angular 2", topics: 70, url: "/angular2/" },
      ]
    };
  },
  computed: {
    filteredLanguages() {
      return this.languages.filter(language =>
        language.name.toLowerCase().includes(this.search.toLowerCase())
      );
    }
  }
};
</script>

<style scoped>
.search {
  text-align: center;
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

.cards {
  padding: 50px 0;
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