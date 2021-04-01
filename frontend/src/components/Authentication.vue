<template>
        <div class="container">
            <h1>Password prompt</h1>
            <form action="">
                <fieldset class="my-3">
                    <label for="filename">File name</label>
                    <input type="text" class="form-control" placeholder="Accounts file name" v-model="filename">
                    <small class="form-text text-muted">the accounts file name</small>

                    <label for="password">Password</label>
                    <input type="password" class="form-control" placeholder="Password" autocomplete="current-password" v-model="password">
                    <small class="form-text text-muted">the password to unlock the accounts database</small>
                </fieldset>
                <button type="button" class="btn btn-outline-primary mx-1" @click.prevent="submit">Submit</button>
            </form>
        </div>
</template>

<script>
export default {
    name: "Authentication",
    data() {
        return {
            password: '',
            filename: "archive.protected"
        }
    },
    props: {
        display: String
    },
    methods: {
        submit: async function(event) {
            try {
                let res = await fetch('/accounts/load', {
                    method: 'POST',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify( { file: this.filename, pwd: this.password })
                })
                let cnt = await res.json()
                console.log( `response was: ${ cnt }` )
                window.location.pathname = '/list'
            } catch(e) {
                console.log(`error: ${e}`)
            }
        }
    }
}
</script>

<style>

</style>