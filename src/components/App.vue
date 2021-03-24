<template>
      <transition name="bounce" appear >
          <component @selected="selectedAccount = $event" :is="currentComponent" :id="selectedAccount" ></component>
    </transition>
</template>

<script>
import { defineAsyncComponent } from 'vue'

// import AccountsList from './AccountsList.vue'
// import AccountShow from './AccountShow.vue'

export default {
  name: 'App',
  data() {
    return {
      selectedAccount: 0
    }
  },
  computed: {
    currentComponent() {
      return this.selectedAccount == 0 ? 'AccountsList': 'AccountShow'
    }
  },
  components: {
    AccountsList : defineAsyncComponent(() => import('./AccountsList.vue')),
    AccountShow: defineAsyncComponent(() => import('./AccountShow.vue'))
    // AccountsList,
    // AccountShow
  }
}
</script>

<style>
* {
  transform: translateZ(0);
}

/* Enter and leave animations can use different */
/* durations and timing functions.              */
.slide-fade-enter-active {
  transition: all 0.3s ease-out;
}

.slide-fade-leave-active {
  transition: all 0.8s cubic-bezier(1, 0.5, 0.8, 1);
}

.slide-fade-enter-from,
.slide-fade-leave-to {
  transform: translateX(20px);
  opacity: 0;
}

.bounce-enter-active {
  animation: bounce-in 0.5s;
}
.bounce-leave-active {
  animation: bounce-in 0.5s reverse;
}
@keyframes bounce-in {
  0% {
    transform: scale(0);
  }
  50% {
    transform: scale(1.25);
  }
  100% {
    transform: scale(1);
  }
}


</style>
