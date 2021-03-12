<template>
  <div class="mb-3">
    <label :for="name" class="form-label">{{ label }}</label>
    <input type="text" class="form-control" :id="name" aria-describedby="`{ name }Help`">
    <div id="`{ name }Help`" class="form-text">{{ help }}</div>
  </div>
</template>

<script>
export default {
  name: 'Field',
  props: {
    name: String,
    label: String,
    help: String
  }
}
</script>

<style scoped>

</style>
