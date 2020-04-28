<template>
  <div>
    <div class="search">
      <input type="text" v-model="search" />
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
        { id: 3, name: "JavaScript", topics: 106, url: "/javascript/" }
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
  width: 300px;
  font-size: 20px;
  padding: 3px;
  outline: 1px solid #42b983;
  border: 1px solid #42b983;
}

.search input:focus {
  outline: 1px solid #42b983;
  border: 1px solid #42b983;
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