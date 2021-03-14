<template>
  <div class="container">
      <h1>Password prompt</h1>
      <form action="">
          <fieldset class="my-3">
            <label for="password">Password</label>
            <input type="password" class="form-control" placeholder="Password" autocomplete="current-password" :value="password">
            <small class="form-text text-muted">the password to unlock the accounts database</small>
            <label for="filename">File name</label>
            <input type="text" class="form-control" placeholder="Accounts file name" :value="filename">
            <small class="form-text text-muted">the accounts file name</small>
          </fieldset>
          <button type="button" class="btn btn-outline-primary mx-1" v-on:click="submit">Submit</button>
      </form>
  </div>
</template>

<script>
export default {
    data() {
        return {
            password: '',
            filename: "accounts.protected"
        }
    }, 
    methods: {
        submit: async function(event) {
            try {
                let res = await fetch('http://localhost/login', {
                method: 'POST',
                body:  {password: this.password}
            })
            let cnt = await res.json()
            // todo parse response
            } catch(e) {

            }
            finally {
                this.$emit('authenticated')
            }
        }
    },
    emits: ['authenticated']
}
</script>

<style>

</style>