<template>
  <div>
    <input type="text" v-model="search" />
    <ul>
      <li v-for="language in filteredLanguages" :key="language.id">{{language.name}}</li>
    </ul>
  </div>
</template>

<script>
export default {
  data() {
    return {
      search: "",
      languages: [
        { id: 1, name: "Stackoverflow" },
        { id: 2, name: "Game of Thrones" },
        { id: 3, name: "Jon Snow" }
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
li {
  color: red;
}
</style>