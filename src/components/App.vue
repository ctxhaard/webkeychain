<template>
  <div id>
    <component @selected="selectedAccount = $event" :is="currentComponent" :id="selectedAccount" ></component>
  </div>
</template>

<script>
import AccountsList from './AccountsList.vue'
import AccountShow from './AccountShow.vue'

export default {
  name: 'App',
  data() {
    return {
      selectedAccount: 0
    }
  },
  computed: {
    currentComponent() {
      return this.selectedAccount == 0 ? 'AccountsList': 'AccountShow'
    }
  },
  components: {
    AccountsList,
    AccountShow
  }
}
</script>

<style>
</style>
