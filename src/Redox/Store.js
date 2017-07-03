"use strict"

function Store(state) {
  this.state = state
  this.lastSubscriptionId = 0
  this.subscriptions = []
}
Store.prototype.subscribe = function(fn) {
  this.subscriptions.push({
    subscription: fn,
    subscriptionId: ++this.lastSubscriptionId
  })
  return this.lastSubscriptionId
}
Store.prototype.unsubscribe = function(subscriptionId) {
  this.subscriptions = this.subscriptions.filter(function(sub) {
    return sub.subscriptionId !== subscriptionId
  })
  return {}
}

exports.mkStore = function mkStore(state) {
  return function() {
    return new Store(state)
  }
}

exports.getState = function(store) {
  return function() {
    return store.state
  }
}

exports.setState = function(store) {
  return function(state) {
    return function() {
      store.state = state
      return store
    }
  }
}

exports.getSubscriptions = function(store) {
  return function() {
    return store.subscriptions.map(function(s) {return s.subscription})
  }
}

exports._subscribe = function(store) {
  return function(fn) {
    return function() {
      return store.subscribe(fn)
    }
  }
}

exports._unsubscribe = function(store) {
  return function(subscriptionId) {
    return function() {
      store.unsubscribe(subscriptionId)
      return {}
    }
  }
}

exports.modifyStore = function(fn) {
  return function(store) {
    return function() {
      store.state = fn(store.state)
      return store
    }
  }
}
