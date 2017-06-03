goog.provide('muancefx.interop');

/*
 * Used to call a java method by name from nashorn
 * I could not find a cljs syntax for it
 */
muancefx.interop.invokePropertyGetter = function(obj, methodName) {
  return obj[methodName]();
};
