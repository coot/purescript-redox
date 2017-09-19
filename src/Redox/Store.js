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

exports.mkStoreImpl = function mkStore(state) {
  return new Store(state)
}

exports.getStateImpl = function(store) {
  return store.state
}

exports.setStateImpl = function(store, state) {
  store.state = state
  return store
}

exports.getSubscriptionsImpl = function(store) {
  return store.subscriptions.map(function(s) {return s.subscription})
}

exports.subscribeImpl = function(store, fn) {
  return store.subscribe(fn)
}

exports.unsubscribeImpl = function(store, subscriptionId) {
  store.unsubscribe(subscriptionId)
}

exports.modifyStoreImpl = function(fn, store) {
  store.state = fn(store.state)
  return store
}
