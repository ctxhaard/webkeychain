<template>
  <div class="container">
    <h1>Accounts</h1>
    <div class="list-group">
        <a href="#" class="list-group-item list-group-item-action" @click.prevent="$emit('selected',account)" v-for="account in accounts" :key="account.id">{{account.id}}: {{account.title}}</a>
    </div>
    <div class="my-3">
      <form action="">
        <button type="button" class="btn btn-outline-info mx-1" @click.prevent="createNew" >Create new</button>
        <button type="button" class="btn btn-outline-primary mx-1" @click.prevent="logout">Logout</button>
      </form>
    </div>
  </div>
</template>

<script>
import { createApp } from 'vue'

export default {
    name: 'AccountsList',
    props: {
        display: String
    },
    emits: ['selected'],
    data() {
        return {
            accounts: []
        }
    },
    methods: {
        async createNew() {
            const res = await fetch('/accounts/new', {
                method: 'GET',
                headers: { 'accept': 'application/json' }
                })
            let json = await res.json()
            this.$emit('selected', json.data)
            // TODO: fetch the account and pass it to
            // the AccountShow, or change the whole logic
            // : make accountslist fetch single accounts
            // ad pass to the accountshow, instead of the
            // id only
        },
        logout() {
            console.log('TODO: add logout js code')
        }
    },
    async mounted () {
        try {
            const res = await fetch('/accounts', {
                method: 'GET'})
            const accounts = await res.json()
            this.accounts = accounts.data         
        } catch (error) {
            console.log(`error: {error} while fetching accounts`)
        }
    }
}
</script>

<style scoped>

</style>
