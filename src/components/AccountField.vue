<template>
  <div class="mb-3">
    <label :for="name" class="form-label">{{ label }}</label>
    <input type="text" class="form-control" :id="name" aria-describedby="`{ name }Help`" :value="value" :placeholder="help">
  </div>
</template>

<script>
export default {
  name: 'AccountField',
  props: {
    name: String,
    label: String,
    help: String,
    value: String
  }, data() {
    return {

    }
  }
}
</script>

<style scoped>

</style>
