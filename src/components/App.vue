<template>
  <div id>
    <AccountsList></AccountsList>
    <AccountShow></AccountShow>
  </div>
</template>

<script>
import AccountsList from './AccountsList.vue'
import AccountShow from './AccountShow.vue'

export default {
  name: 'App',
  components: {
    AccountsList,
    AccountShow
  }
}
</script>

<style>
</style>
