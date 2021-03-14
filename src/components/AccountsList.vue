<template>
    <div class="container">
        <h1>Accounts</h1>
        <div class="list-group">
            <a href="#" class="list-group-item list-group-item-action" @click.prevent="onSelected(account.id)" v-for="account in accounts" :key="account.id">{{account.id}}: {{account.title}}</a>
        </div>
    </div>
    <div id="mount-point-2"></div>
</template>

<script>
import AccountShow from '/src/components/AccountShow.vue'
import { createApp } from 'vue'

export default {
    data() {
        return {
            accounts: []
        }
    },
    methods: {
        onSelected(id) {
            console.log(`account: ${id} was selected`)
            // TODO: 
            // disable list items selection
            // blur the list
            // show the accounts detail
            createApp(AccountShow).mount('#mount-point-2')
        }
    },
    emits: ['selected'],
    async created () {
        try {
            const res = await fetch('http://localhost:3000/api/accounts', {
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
