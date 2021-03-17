import 'bootstrap'
import 'bootstrap/dist/css/bootstrap.min.css'
// dark theme
import '@forevolve/bootstrap-dark/dist/css/bootstrap-dark.min.css'


import { createApp, h } from 'vue'

import App from '/src/App.vue'
import Authentication from '/src/components/Authentication.vue'

const routes = {
    '/': Authentication,
    '/list': App
  }

const SimpleRouterApp = {
  data() {
      console.log(`current route: ${window.location.pathname}`)
      return {currentRoute: window.location.pathname}
  },
  computed: {
    ViewComponent () {
      const matchingPage = routes[this.currentRoute] || Authentication
      return matchingPage
    }
  },

  render () {
    return h(this.ViewComponent)
  },

  created () {
    window.addEventListener('popstate', () => {
      this.currentRoute = window.location.pathname
    })
  }
}

createApp(SimpleRouterApp).mount('#app')
