<template>
  <div class="container">
    <h1>Account editor</h1>
    <form action="">
      <fieldset :disabled="disabled">
        <AccountField name="name" label="Account name" help="the account name" v-model="account.title"></AccountField>
        <AccountField name="url" label="URL" help="the account URL" v-model="account.url"></AccountField>
        <AccountField name="username" label="Username" help="the account username" v-model="account.username" ></AccountField>
        <AccountField name="password" label="Password" :hide="hidePassword" help="the account password" v-model="account.password" ></AccountField>
        <AccountField name="notes" label="Notes" help="the account notes" v-model="account.notes" ></AccountField>
      </fieldset>
      <div class="mb-3">
        <button type="button" class="btn btn-outline-info mx-1" @click.prevent="hidePassword = !hidePassword" >Show password</button>
        <button type="button" class="btn btn-outline-primary mx-1" @click.prevent="disabled = false">Edit</button>
        <button type="button" class="btn btn-outline-danger mx-1" @click.prevent="saveAccount" >Save</button>
        <button type="button" class="btn btn-outline-secondary mx-1" @click.prevent="$emit('selected',0)" >Cancel</button>  
        <button type="button" class="btn btn-outline-danger mx-10" @click.prevent="deleteAccount" >Delete</button>
      </div>
    </form>
  </div>
</template>

<script>
import AccountField from './AccountField.vue'
export default {
    name: 'AccountShow',
    props: {
      selected: Object
    },
    emits: [ 'selected' ],
    components: {
        AccountField
    },
    data() {
      return {
        account: {
          title: "",
          url: "",
          username: "",
          password: "",
          notes: "",
          id: 0
         },
         hidePassword: true,
         disabled: true
      }
    },
    methods: {
      async saveAccount() {
        let res = await fetch('/accounts/' + this.account.id, {
          method: 'PATCH',
          headers: { 'content-type': 'application/json' },
          body: JSON.stringify(this.account)
        })
        this.$emit('selected', {})
      },
      async deleteAccount() {
        let res = await fetch('/accounts/' + this.account.id, {
          method: 'DELETE'
        })
        this.$emit('selected', {})
      }
    },
    mounted() {
      this.account = this.selected
    }
}
</script>

<style scoped>

</style>