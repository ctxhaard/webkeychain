<template>
    <div class="container">
        <h1>Accounts</h1>
        <div class="list-group">
            <a href="#" class="list-group-item list-group-item-action" @click.prevent="$emit('selected',account.id)" v-for="account in accounts" :key="account.id">{{account.id}}: {{account.title}}</a>
        </div>
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
