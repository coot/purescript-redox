"use strict"

function Store(state) {
  this.state = state
  this.subscriptions = []
}
Store.prototype.subscribe = function(fn) {
  this.subscriptions.push(fn)
}
Store.prototype.unsubscribe = function(fn) {
  this.subscriptions = this.subscriptions.filter(function(fn_) {return fn_ !== fn})
}

exports.createStore = function createStore(state) {
  return function() {
    return new Store(state)
  }
}

exports.getState = function(store) {
  return store.state
}

exports.subscribe = function(store) {
  return function(fn) {
    return function() {
      store.subscribe(fn)
      return {}
    }
  }
}

exports.unsubscribe = function(store) {
  return function(fn) {
    return function() {
      store.unsubscribe(fn)
      return {}
    }
  }
}

exports.mapStore = function(store) {
  return function(fn) {
    return function() {
      store.state = fn(store.state)
    }
  }
}
