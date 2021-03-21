<template>
  <div class="mb-3">
    <label :for="name" class="form-label">{{ label }}</label>
    <input :type="inputType" class="form-control" :id="name" :aria-describedby="name + 'Help'" :value="value" :placeholder="help">
  </div>
</template>

<script>
export default {
  name: 'AccountField',
  inheritAttrs: false,
  props: {
    name: String,
    label: String,
    help: String,
    value: String,
    hide: Boolean
  },
  computed: {
    inputType() {
      return (this.hide ? 'password' : 'text')
    }
  }
}
</script>

<style scoped>

</style>
