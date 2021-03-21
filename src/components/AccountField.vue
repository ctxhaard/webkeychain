<template>
  <div class="mb-3">
    <label :for="name" class="form-label">{{ label }}</label>
    <input 
      class="form-control" 
      :type="inputType"
      :id="name" 
      :aria-describedby="name + 'Help'"
      :value="modelValue"
      @input="$emit('update:modelValue', $event.target.value)" 
      :placeholder="help">
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
    modelValue: String,
    hide: Boolean
  },
  emits: [ 'update:modelValue' ],
  computed: {
    inputType() {
      return (this.hide ? 'password' : 'text')
    }
  }
}
</script>

<style scoped>

</style>
