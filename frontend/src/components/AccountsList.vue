<template>
  <div class="container">
    <h1>Accounts</h1>
    <div class="my-3">
      <form action="">
        <button type="button" class="btn btn-outline-info mx-1" @click.prevent="createNew" >Create new</button>
        <button type="button" class="btn btn-outline-primary mx-1" @click.prevent="logout">Logout</button>
        <input type="text" class="form-control" placeholder="accounts filter" @input="applyFilter" >
      </form>
    </div>
    <div class="list-group">
        <a href="#" class="list-group-item list-group-item-action" @click.prevent="$emit('selected',account)" v-for="account in accountsFiltered" :key="account.id">{{account.id}}: {{account.title}}</a>
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
            accounts: [],
            filter: '.*'
        }
    },
    computed: {
        accountsFiltered() {
            let filter = this.filter
            return this.accounts.filter(function (account) {
                let re = new RegExp(`${filter}`,'i')
                for( const prop in account) {
                    if (re.test(account[prop])) return true
                }
                return false
            })
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
        async logout() {
            const res = await fetch('/accounts/unload', {
                method: 'POST',
                headers: { 'accept': 'application/json' }
            })
            let json = await res.json()
            window.location.pathname = '/'
        },
        applyFilter(event) {
            console.log(`Current filter is: ${ event.srcElement.value }`)
            this.filter = event.srcElement.value
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
