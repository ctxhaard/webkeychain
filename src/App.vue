<template>
  <div id>
    <AccountsList></AccountsList>
  </div>
</template>

<script>
import AccountsList from '/src/components/AccountsList.vue'
import { createApp } from 'vue'

export default {
  name: 'App',
  components: {
    AccountsList
  }
}
</script>

<style>
</style>
