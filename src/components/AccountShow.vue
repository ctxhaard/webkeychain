<template>
  <div class="container">
    <h1>Account editor</h1>
    <form action="">
      <fieldset disabled>
        <AccountField name="name" label="Account name" help="the account name" :value="account.title"></AccountField>
        <AccountField name="url" label="URL" help="the account URL" :value="account.url"></AccountField>
        <AccountField name="username" label="Username" help="the account username" :value="account.username" ></AccountField>
        <AccountField name="password" label="Password" :hide="hidePassword" help="the account password" :value="account.password" ></AccountField>
        <AccountField name="notes" label="Notes" help="the account notes" :value="account.notes" ></AccountField>
      </fieldset>
      <div class="mb-3">
        <button type="button" class="btn btn-outline-info mx-1" @click.prevent="hidePassword = !hidePassword" >Show password</button>
        <button type="button" class="btn btn-outline-primary mx-1">Edit</button>
        <button type="button" class="btn btn-outline-danger mx-1">Save</button>
        <button type="button" class="btn btn-outline-secondary mx-1" @click.prevent="$emit('selected',0)" >Cancel</button>  
      </div>
    </form>
  </div>
</template>

<script>
import AccountField from './AccountField.vue'
export default {
    name: 'AccountShow',
    components: {
        AccountField
    },
    emits: [ 'selected' ],
    props: {
      id: Number
    },
    data() {
      return {
        account: {
          title: "",
          url: "",
          username: "",
          password: "",
          notes: ""
         },
         hidePassword: true
      }
    },
    async mounted() {
      try {
        let res = await fetch('/accounts/' + this.id, {
          method: 'GET',
          headers: { accpet: 'application/json' }
        })
        let json = await res.json()
        this.account = json.data
      } catch(e) {
        console.log(`Error: ${e} while getting account: ${this.id}`)
      }
    }
}
</script>

<style scoped>

</style>