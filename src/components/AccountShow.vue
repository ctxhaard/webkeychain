<template>
  <div class="container">
    <h1>Account editor</h1>
    <form action="">
      <fieldset disabled>
        <AccountField name="name" label="Account name" help="the account name" :value="account.name"></AccountField>
        <AccountField name="url" label="URL" help="the account URL" :value="account.url"></AccountField>
        <AccountField name="username" label="Username" help="the account username" :value="account.username" ></AccountField>
        <AccountField name="password" label="Password" help="the account password" :value="account.password" ></AccountField>
        <AccountField name="notes" label="Notes" help="the account notes" :value="account.notes" ></AccountField>
      </fieldset>
      <div class="mb-3">
        <button type="button" class="btn btn-outline-info mx-1">Show password</button>
        <button type="button" class="btn btn-outline-primary mx-1">Edit</button>
        <button type="button" class="btn btn-outline-danger mx-1">Save</button>  
      </div>
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