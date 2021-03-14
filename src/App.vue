<template>
  <div id="mount-point">
    <Authentication @authenticated="onAuthenticated" ></Authentication>
  </div>
</template>

<script>
import Authentication from '/src/components/Authentication.vue'
import AccountList from '/src/components/AccountsList.vue'
import { createApp } from 'vue'

export default {
  name: 'App',
  components: {
    Authentication,
  },
  methods: {
    onAuthenticated () {
      console.log("authenticated event captured!")
      createApp(AccountList).mount('#mount-point')
    }
  }
}
</script>

<style>
</style>
