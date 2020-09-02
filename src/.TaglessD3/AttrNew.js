"use strict";

// module AttrNew

exports.uncurryAttrSetter = function(cssunit) {
    return function (fn) {
      /* jshint maxparams: 4 */
      return function (a, b, c, d) {
        var value =  fn(a)(b)(c)(d)
        return value + cssunit;
      };
    };
};
