<template>
    <div class="container">
        <h1>Accounts</h1>
        <div class="list-group">
            <a href="#" class="list-group-item list-group-item-action" @click.prevent="$emit('selected',account.id)" v-for="account in accounts" :key="account.id">{{account.id}}: {{account.title}}</a>
        </div>
            <form action="">
            <div class="mb-3">
                <button type="button" class="btn btn-outline-info mx-1" @click.prevent="createNew" >Create new</button>
                <button type="button" class="btn btn-outline-primary mx-1" @click.prevent="logout">Logout</button>
            </div>
            </form>

    </div>
</template>

<script>
import { createApp } from 'vue'

export default {
    name: 'AccountsList',
    data() {
        return {
            accounts: []
        }
    },
    props: {
        display: String
    },
    methods: {
        createNew() {
            console.log('TODO: add create new account js code')
        },
        logout() {
            console.log('TODO: add logout js code')
        }
    },
    emits: ['selected'],
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
