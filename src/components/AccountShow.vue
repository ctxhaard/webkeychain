<template>
  <div class="container">
    <h1>Account editor</h1>
    <form action="">
      <fieldset disabled>
        <AccountField name="name" label="Account name" help="the account name">{{ account.name }}</AccountField>
        <AccountField name="url" label="URL" help="the account URL">{{ account.url }}</AccountField>
        <AccountField name="username" label="Username" help="the account username">{{ account.username }}</AccountField>
        <AccountField name="password" label="Password" help="the account password">{{ account.password }}</AccountField>
        <AccountField name="notes" label="Notes" help="the account notes">{{ account.notes }}</AccountField>
        <div class="mb-3">
          <button class="btn btn-primary mx-1">Show password</button>
          <button class="btn btn-primary mx-1">Edit</button>
          <button type="submit" class="btn btn-primary mx-1">Save</button>  
        </div>
      </fieldset>
    </form>
  </div>
</template>

<script>
import AccountField from './AccountField.vue'
export default {
    components: {
        AccountField
    },
    data() {
      return {
        account: {
          name: 'Google',
          url: 'http://www.google.com',
          username: 'mario.rossi@google.com',
          password: 'acme123',
          notes: 'these are the account notes'
      }}
    }
}
</script>

<style scoped>

</style>