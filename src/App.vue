<template>
 <div class="container">
  <nav class="navbar navbar-expand-lg navbar-dark bg-dark">
    <div class="navbar navbar-collapse">
      <ul class="navbar-nav mr-auti">
        <li class="nav-item mr-3">Accounts list</li>
        <li class="nav-item mr-3">Edit account</li>
      </ul>
    </div>
  </nav>
 </div>
  <AccountShow></AccountShow>
  <AccountsList></AccountsList>
</template>

<script>
import AccountShow from './components/AccountShow.vue'
import AccountsList from './components/AccountsList'

export default {
  name: 'App',
  components: {
    AccountShow,
    AccountsList
  }
}
</script>

<style>
</style>
