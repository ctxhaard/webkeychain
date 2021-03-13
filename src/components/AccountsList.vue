<template>
    <div class="container">
        <h1>Accounts</h1>
        <div class="list-group">
            <a href="#" class="list-group-item list-group-item-action" v-for="account in accounts" :key="account.id">{{ account.name }}</a>
        </div>
    </div>
</template>

<script>
export default {
    data() {
        return {
            accounts: []
        }
    }, 
    async created () {
        try {
            const res = await fetch('http://localhost:3000/accounts', {
                method: 'GET'})
            const accounts = await res.json()
            this.accounts = accounts            
        } catch (error) {
            console.log(`error: {error} while fetching accounts`)
        }
    }
}
</script>

<style scoped>

</style>
