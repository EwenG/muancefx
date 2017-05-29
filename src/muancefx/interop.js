goog.provide('muancefx.interop');

/*
 * Used to call a java method by name from nashorn
 * I could not find a cljs syntax for it
 */
muancefx.interop.invoke = function(obj, fnName, param) {
  return obj[fnName](param);
};
