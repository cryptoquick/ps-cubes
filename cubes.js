(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}],2:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Color = require("../Color");
var yellow = Color.rgb(255)(235)(59);
var teal = Color.rgb(0)(150)(136);
var red = Color.rgb(244)(67)(54);
var purple = Color.rgb(156)(39)(176);
var pink = Color.rgb(233)(30)(99);
var orange = Color.rgb(255)(152)(0);
var lime = Color.rgb(205)(220)(57);
var lightGreen = Color.rgb(139)(195)(74);
var lightBlue = Color.rgb(3)(169)(244);
var indigo = Color.rgb(63)(81)(181);
var grey = Color.rgb(158)(158)(158);
var green = Color.rgb(76)(175)(80);
var deepPurple = Color.rgb(103)(58)(183);
var deepOrange = Color.rgb(255)(87)(34);
var cyan = Color.rgb(0)(188)(212);
var brown = Color.rgb(121)(85)(72);
var blueGrey = Color.rgb(96)(125)(139);
var blue = Color.rgb(33)(150)(243);
var amber = Color.rgb(255)(193)(7);
module.exports = {
    amber: amber, 
    blue: blue, 
    blueGrey: blueGrey, 
    brown: brown, 
    cyan: cyan, 
    deepOrange: deepOrange, 
    deepPurple: deepPurple, 
    green: green, 
    grey: grey, 
    indigo: indigo, 
    lightBlue: lightBlue, 
    lightGreen: lightGreen, 
    lime: lime, 
    orange: orange, 
    pink: pink, 
    purple: purple, 
    red: red, 
    teal: teal, 
    yellow: yellow
};

},{"../Color":3}],3:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int = require("../Data.Int");
var Data_Int_Bits = require("../Data.Int.Bits");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex = require("../Data.String.Regex");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var RGB = (function () {
    function RGB() {

    };
    RGB.value = new RGB();
    return RGB;
})();
var HSL = (function () {
    function HSL() {

    };
    HSL.value = new HSL();
    return HSL;
})();
var LCh = (function () {
    function LCh() {

    };
    LCh.value = new LCh();
    return LCh;
})();
var Lab = (function () {
    function Lab() {

    };
    Lab.value = new Lab();
    return Lab;
})();
var HSLA = (function () {
    function HSLA(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    HSLA.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new HSLA(value0, value1, value2, value3);
                };
            };
        };
    };
    return HSLA;
})();
var toRGBA$prime = function (v) {
    var h$prime = v.value0 / 60.0;
    var chr = (1.0 - $$Math.abs(2.0 * v.value2 - 1.0)) * v.value1;
    var m = v.value2 - chr / 2.0;
    var x = chr * (1.0 - $$Math.abs($$Math.remainder(h$prime)(2.0) - 1.0));
    var col = (function () {
        if (h$prime < 1.0) {
            return {
                r: chr, 
                g: x, 
                b: 0.0
            };
        };
        if (1.0 <= h$prime && h$prime < 2.0) {
            return {
                r: x, 
                g: chr, 
                b: 0.0
            };
        };
        if (2.0 <= h$prime && h$prime < 3.0) {
            return {
                r: 0.0, 
                g: chr, 
                b: x
            };
        };
        if (3.0 <= h$prime && h$prime < 4.0) {
            return {
                r: 0.0, 
                g: x, 
                b: chr
            };
        };
        if (4.0 <= h$prime && h$prime < 5.0) {
            return {
                r: x, 
                g: 0.0, 
                b: chr
            };
        };
        if (Data_Boolean.otherwise) {
            return {
                r: chr, 
                g: 0.0, 
                b: x
            };
        };
        throw new Error("Failed pattern match at Color line 295, column 26 - line 306, column 61: " + [  ]);
    })();
    return {
        r: col.r + m, 
        g: col.g + m, 
        b: col.b + m, 
        a: v.value3
    };
};
var toXYZ = function (c) {
    var rec = toRGBA$prime(c);
    var finv = function (c$prime) {
        if (c$prime <= 4.045e-2) {
            return c$prime / 12.92;
        };
        if (Data_Boolean.otherwise) {
            return $$Math.pow((c$prime + 5.5e-2) / 1.055)(2.4);
        };
        throw new Error("Failed pattern match at Color line 314, column 11 - line 328, column 1: " + [ c$prime.constructor.name ]);
    };
    var g = finv(rec.g);
    var r = finv(rec.r);
    var b = finv(rec.b);
    var x = 0.4124 * r + 0.3576 * g + 0.1805 * b;
    var y = 0.2126 * r + 0.7152 * g + 7.22e-2 * b;
    var z = 1.93e-2 * r + 0.1192 * g + 0.9505 * b;
    return {
        x: x, 
        y: y, 
        z: z
    };
};
var toRGBA = function (v) {
    var c = toRGBA$prime(v);
    var g = Data_Int.round(255.0 * c.g);
    var r = Data_Int.round(255.0 * c.r);
    var b = Data_Int.round(255.0 * c.b);
    return {
        r: r, 
        g: g, 
        b: b, 
        a: v.value3
    };
};
var toHexString = function (color) {
    var toHex = function (num) {
        var repr = Data_Int.toStringAs(Data_Int.hexadecimal)(num);
        var $27 = Data_String.length(repr) === 1;
        if ($27) {
            return "0" + repr;
        };
        return repr;
    };
    var c = toRGBA(color);
    return "#" + (toHex(c.r) + (toHex(c.g) + toHex(c.b)));
};
var toHSLA = function (v) {
    return {
        h: v.value0, 
        s: v.value1, 
        l: v.value2, 
        a: v.value3
    };
};
var showColor = new Data_Show.Show(function (c) {
    var col = toRGBA(c);
    return "rgba " + (Data_Show.show(Data_Show.showInt)(col.r) + (" " + (Data_Show.show(Data_Show.showInt)(col.g) + (" " + (Data_Show.show(Data_Show.showInt)(col.b) + (" " + Data_Show.show(Data_Show.showNumber)(col.a)))))));
});
var modPos = function (x) {
    return function (y) {
        return $$Math.remainder($$Math.remainder(x)(y) + y)(y);
    };
};
var rgba = function (red$prime) {
    return function (green$prime) {
        return function (blue$prime) {
            return function (alpha) {
                var red = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(red$prime);
                var r = Data_Int.toNumber(red) / 255.0;
                var green = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(green$prime);
                var g = Data_Int.toNumber(green) / 255.0;
                var blue = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(blue$prime);
                var maxChroma = Data_Ord.max(Data_Ord.ordInt)(Data_Ord.max(Data_Ord.ordInt)(red)(green))(blue);
                var minChroma = Data_Ord.min(Data_Ord.ordInt)(Data_Ord.min(Data_Ord.ordInt)(red)(green))(blue);
                var chroma = maxChroma - minChroma | 0;
                var chroma$prime = Data_Int.toNumber(chroma) / 255.0;
                var lightness = Data_Int.toNumber(maxChroma + minChroma | 0) / (255.0 * 2.0);
                var saturation = (function () {
                    if (chroma === 0) {
                        return 0.0;
                    };
                    if (Data_Boolean.otherwise) {
                        return chroma$prime / (1.0 - $$Math.abs(2.0 * lightness - 1.0));
                    };
                    throw new Error("Failed pattern match at Color line 118, column 32 - line 146, column 75: " + [  ]);
                })();
                var b = Data_Int.toNumber(blue) / 255.0;
                var hue$prime = function (v) {
                    if (v === 0) {
                        return 0.0;
                    };
                    if (maxChroma === red) {
                        return modPos((g - b) / chroma$prime)(6.0);
                    };
                    if (maxChroma === green) {
                        return (b - r) / chroma$prime + 2.0;
                    };
                    if (Data_Boolean.otherwise) {
                        return (r - g) / chroma$prime + 4.0;
                    };
                    throw new Error("Failed pattern match at Color line 118, column 32 - line 146, column 75: " + [ v.constructor.name ]);
                };
                var hue = 60.0 * hue$prime(chroma);
                return new HSLA(hue, saturation, lightness, alpha);
            };
        };
    };
};
var rgb = function (r) {
    return function (g) {
        return function (b) {
            return rgba(r)(g)(b)(1.0);
        };
    };
};
var rgba$prime = function (r) {
    return function (g) {
        return function (b) {
            return function (a) {
                return rgba(Data_Int.round(r * 255.0))(Data_Int.round(g * 255.0))(Data_Int.round(b * 255.0))(a);
            };
        };
    };
};
var rgb$prime = function (r) {
    return function (g) {
        return function (b) {
            return rgba$prime(r)(g)(b)(1.0);
        };
    };
};
var xyz = function (x) {
    return function (y) {
        return function (z) {
            var f = function (c) {
                if (c <= 3.1308e-3) {
                    return 12.92 * c;
                };
                if (Data_Boolean.otherwise) {
                    return 1.055 * $$Math.pow(c)(1.0 / 2.4) - 5.5e-2;
                };
                throw new Error("Failed pattern match at Color line 193, column 5 - line 193, column 50: " + [ c.constructor.name ]);
            };
            var g = f(-0.9689 * x + 1.8758 * y + 4.15e-2 * z);
            var r = f(3.2406 * x - 1.5372 * y - 0.4986 * z);
            var b = f((5.57e-2 * x - 0.204 * y) + 1.057 * z);
            return rgb$prime(r)(g)(b);
        };
    };
};
var luminance = function (col) {
    var val = toRGBA$prime(col);
    var f = function (c) {
        if (c <= 3.928e-2) {
            return c / 12.92;
        };
        if (Data_Boolean.otherwise) {
            return $$Math.pow((c + 5.5e-2) / 1.055)(2.4);
        };
        throw new Error("Failed pattern match at Color line 521, column 17 - line 529, column 26: " + [ c.constructor.name ]);
    };
    var g = f(val.g);
    var r = f(val.r);
    var b = f(val.b);
    return 0.2126 * r + 0.7152 * g + 7.22e-2 * b;
};
var interpolate = function (fraction) {
    return function (a) {
        return function (b) {
            return a + fraction * (b - a);
        };
    };
};
var interpolateAngle = function (fraction) {
    return function (a) {
        return function (b) {
            var paths = [ {
                from: a, 
                to: b
            }, {
                from: a, 
                to: b + 360.0
            }, {
                from: a + 360.0, 
                to: b
            } ];
            var dist = function (v) {
                return $$Math.abs(v.to - v.from);
            };
            var shortest = Data_Maybe.fromJust()(Data_Foldable.minimumBy(Data_Foldable.foldableArray)(Data_Ord.comparing(Data_Ord.ordNumber)(dist))(paths));
            return interpolate(fraction)(shortest.from)(shortest.to);
        };
    };
};
var hsla = function (h) {
    return function (s) {
        return function (l) {
            return function (a) {
                var s$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(s);
                var l$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(l);
                var h$prime = modPos(h)(360.0);
                var a$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(a);
                return new HSLA(h$prime, s$prime, l$prime, a$prime);
            };
        };
    };
};
var lighten = function (f) {
    return function (v) {
        return hsla(v.value0)(v.value1)(v.value2 + f)(v.value3);
    };
};
var rotateHue = function (angle) {
    return function (v) {
        return hsla(v.value0 + angle)(v.value1)(v.value2)(v.value3);
    };
};
var saturate = function (f) {
    return function (v) {
        return hsla(v.value0)(v.value1 + f)(v.value2)(v.value3);
    };
};
var hsl = function (h) {
    return function (s) {
        return function (l) {
            return hsla(h)(s)(l)(1.0);
        };
    };
};
var white = hsl(0.0)(0.0)(1.0);
var graytone = function (l) {
    return hsl(0.0)(0.0)(l);
};
var fromInt = function (m) {
    var n = Data_Ord.clamp(Data_Ord.ordInt)(0)(16777215)(m);
    var r = n >> 16 & 255;
    var g = n >> 8 & 255;
    var b = n & 255;
    return rgb(r)(g)(b);
};
var fromHexString = function (str) {
    var parseHex = function ($79) {
        return Data_Maybe.fromMaybe(0)(Data_Int.fromStringAs(Data_Int.hexadecimal)($79));
    };
    var isShort = Data_String.length(str) === 4;
    var hush = Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
    var pair = "(" + ("[0-9a-f]" + ("[0-9a-f]" + ")"));
    var single = "(" + ("[0-9a-f]" + ")");
    var variant = (function () {
        if (isShort) {
            return single + (single + single);
        };
        return pair + (pair + pair);
    })();
    var mPattern = Data_String_Regex.regex("^#(?:" + (variant + ")$"))(Data_String_Regex.parseFlags("i"));
    return Control_Bind.bind(Data_Maybe.bindMaybe)(hush(mPattern))(function (v) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_String_Regex.match(v)(str))(function (v1) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(1))))(function (v2) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(2))))(function (v3) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(3))))(function (v4) {
                        if (isShort) {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(rgb((16 * v2 | 0) + v2 | 0)((16 * v3 | 0) + v3 | 0)((16 * v4 | 0) + v4 | 0));
                        };
                        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(rgb(v2)(v3)(v4));
                    });
                });
            });
        });
    });
};
var eqColor = new Data_Eq.Eq(function (c1) {
    return function (c2) {
        var rgb2 = toRGBA(c2);
        var rgb1 = toRGBA(c1);
        return rgb1.r === rgb2.r && (rgb1.g === rgb2.g && (rgb1.b === rgb2.b && rgb1.a === rgb2.a));
    };
});
var desaturate = function (f) {
    return saturate(-f);
};
var darken = function (f) {
    return lighten(-f);
};
var d65 = {
    xn: 0.95047, 
    yn: 1.0, 
    zn: 1.08883
};
var lab = function (l) {
    return function (a) {
        return function (b) {
            var l$prime = (l + 16.0) / 116.0;
            var delta = 6.0 / 29.0;
            var finv = function (t) {
                if (t > delta) {
                    return $$Math.pow(t)(3.0);
                };
                if (Data_Boolean.otherwise) {
                    return 3.0 * delta * delta * (t - 4.0 / 29.0);
                };
                throw new Error("Failed pattern match at Color line 212, column 13 - line 221, column 64: " + [ t.constructor.name ]);
            };
            var x = d65.xn * finv(l$prime + a / 500.0);
            var y = d65.yn * finv(l$prime);
            var z = d65.zn * finv(l$prime - b / 200.0);
            return xyz(x)(y)(z);
        };
    };
};
var lch = function (l) {
    return function (c) {
        return function (h) {
            var deg2rad = $$Math.pi / 180.0;
            var b = c * $$Math.sin(h * deg2rad);
            var a = c * $$Math.cos(h * deg2rad);
            return lab(l)(a)(b);
        };
    };
};
var toLab = function (col) {
    var rec = toXYZ(col);
    var cut = $$Math.pow(6.0 / 29.0)(3.0);
    var f = function (t) {
        if (t > cut) {
            return $$Math.pow(t)(1.0 / 3.0);
        };
        if (Data_Boolean.otherwise) {
            return (1.0 / 3.0) * $$Math.pow(29.0 / 6.0)(2.0) * t + 4.0 / 29.0;
        };
        throw new Error("Failed pattern match at Color line 332, column 13 - line 346, column 1: " + [ t.constructor.name ]);
    };
    var fy = f(rec.y / d65.yn);
    var l = 116.0 * fy - 16.0;
    var b = 200.0 * (fy - f(rec.z / d65.zn));
    var a = 500.0 * (f(rec.x / d65.xn) - fy);
    return {
        l: l, 
        a: a, 
        b: b
    };
};
var distance = function (col1) {
    return function (col2) {
        var sq = function (x) {
            return $$Math.pow(x)(2.0);
        };
        var c2 = toLab(col2);
        var c1 = toLab(col1);
        return $$Math.sqrt(sq(c1.l - c2.l) + sq(c1.a - c2.a) + sq(c1.b - c2.b));
    };
};
var toLCh = function (col) {
    var rec = toLab(col);
    var rad2deg = 180.0 / $$Math.pi;
    var c = $$Math.sqrt(rec.a * rec.a + rec.b * rec.b);
    var h = modPos($$Math.atan2(rec.b)(rec.a) * rad2deg)(360.0);
    return {
        l: rec.l, 
        c: c, 
        h: h
    };
};
var mix = function (v) {
    return function (c1) {
        return function (c2) {
            return function (frac) {
                if (v instanceof HSL) {
                    var t = toHSLA(c2);
                    var f = toHSLA(c1);
                    return hsla(interpolateAngle(frac)(f.h)(t.h))(interpolate(frac)(f.s)(t.s))(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.a)(t.a));
                };
                if (v instanceof RGB) {
                    var t = toRGBA$prime(c2);
                    var f = toRGBA$prime(c1);
                    return rgba$prime(interpolate(frac)(f.r)(t.r))(interpolate(frac)(f.g)(t.g))(interpolate(frac)(f.b)(t.b))(interpolate(frac)(f.a)(t.a));
                };
                if (v instanceof LCh) {
                    var t = toLCh(c2);
                    var f = toLCh(c1);
                    return lch(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.c)(t.c))(interpolateAngle(frac)(f.h)(t.h));
                };
                if (v instanceof Lab) {
                    var t = toLab(c2);
                    var f = toLab(c1);
                    return lab(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.a)(t.a))(interpolate(frac)(f.b)(t.b));
                };
                throw new Error("Failed pattern match at Color line 475, column 1 - line 482, column 18: " + [ v.constructor.name, c1.constructor.name, c2.constructor.name, frac.constructor.name ]);
            };
        };
    };
};
var toGray = function (col) {
    var res = toLCh(col);
    return desaturate(1.0)(lch(res.l)(0.0)(0.0));
};
var cssStringRGBA = function (col) {
    var c = toRGBA(col);
    var green = Data_Show.show(Data_Show.showInt)(c.g);
    var red = Data_Show.show(Data_Show.showInt)(c.r);
    var blue = Data_Show.show(Data_Show.showInt)(c.b);
    var alpha = Data_Show.show(Data_Show.showNumber)(c.a);
    var $70 = c.a === 1.0;
    if ($70) {
        return "rgb(" + (red + (", " + (green + (", " + (blue + ")")))));
    };
    return "rgba(" + (red + (", " + (green + (", " + (blue + (", " + (alpha + ")")))))));
};
var cssStringHSLA = function (v) {
    var toString = function (n) {
        return Data_Show.show(Data_Show.showNumber)(Data_Int.toNumber(Data_Int.round(100.0 * n)) / 100.0);
    };
    var saturation = toString(v.value1 * 100.0) + "%";
    var lightness = toString(v.value2 * 100.0) + "%";
    var hue = toString(v.value0);
    var alpha = Data_Show.show(Data_Show.showNumber)(v.value3);
    var $72 = v.value3 === 1.0;
    if ($72) {
        return "hsl(" + (hue + (", " + (saturation + (", " + (lightness + ")")))));
    };
    return "hsla(" + (hue + (", " + (saturation + (", " + (lightness + (", " + (alpha + ")")))))));
};
var contrast = function (c1) {
    return function (c2) {
        var l2 = luminance(c2);
        var l1 = luminance(c1);
        var $77 = l1 > l2;
        if ($77) {
            return (l1 + 5.0e-2) / (l2 + 5.0e-2);
        };
        return (l2 + 5.0e-2) / (l1 + 5.0e-2);
    };
};
var isReadable = function (c1) {
    return function (c2) {
        return contrast(c1)(c2) > 4.5;
    };
};
var complementary = rotateHue(180.0);
var brightness = function (col) {
    var c = toRGBA$prime(col);
    return (299.0 * c.r + 587.0 * c.g + 114.0 * c.b) / 1000.0;
};
var isLight = function (c) {
    return brightness(c) > 0.5;
};
var black = hsl(0.0)(0.0)(0.0);
var textColor = function (c) {
    if (isLight(c)) {
        return black;
    };
    if (Data_Boolean.otherwise) {
        return white;
    };
    throw new Error("Failed pattern match at Color line 566, column 1 - line 567, column 32: " + [ c.constructor.name ]);
};
module.exports = {
    RGB: RGB, 
    HSL: HSL, 
    LCh: LCh, 
    Lab: Lab, 
    black: black, 
    brightness: brightness, 
    complementary: complementary, 
    contrast: contrast, 
    cssStringHSLA: cssStringHSLA, 
    cssStringRGBA: cssStringRGBA, 
    darken: darken, 
    desaturate: desaturate, 
    distance: distance, 
    fromHexString: fromHexString, 
    fromInt: fromInt, 
    graytone: graytone, 
    hsl: hsl, 
    hsla: hsla, 
    isLight: isLight, 
    isReadable: isReadable, 
    lab: lab, 
    lch: lch, 
    lighten: lighten, 
    luminance: luminance, 
    mix: mix, 
    rgb: rgb, 
    "rgb'": rgb$prime, 
    rgba: rgba, 
    "rgba'": rgba$prime, 
    rotateHue: rotateHue, 
    saturate: saturate, 
    textColor: textColor, 
    toGray: toGray, 
    toHSLA: toHSLA, 
    toHexString: toHexString, 
    toLCh: toLCh, 
    toLab: toLab, 
    toRGBA: toRGBA, 
    "toRGBA'": toRGBA$prime, 
    toXYZ: toXYZ, 
    white: white, 
    xyz: xyz, 
    showColor: showColor, 
    eqColor: eqColor
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Semigroupoid":42,"../Data.Array":53,"../Data.Boolean":63,"../Data.Either":75,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Int":101,"../Data.Int.Bits":99,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.String":139,"../Data.String.Regex":135,"../Math":166,"../Partial.Unsafe":168,"../Prelude":171}],4:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Functor = require("../Data.Functor");
var Data_Semigroup = require("../Data.Semigroup");
var Alt = function (Functor0, alt) {
    this.Functor0 = Functor0;
    this.alt = alt;
};
var altArray = new Alt(function () {
    return Data_Functor.functorArray;
}, Data_Semigroup.append(Data_Semigroup.semigroupArray));
var alt = function (dict) {
    return dict.alt;
};
module.exports = {
    Alt: Alt, 
    alt: alt, 
    altArray: altArray
};

},{"../Data.Functor":91,"../Data.Semigroup":128}],5:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Alternative = function (Applicative0, Plus1) {
    this.Applicative0 = Applicative0;
    this.Plus1 = Plus1;
};
var alternativeArray = new Alternative(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Plus.plusArray;
});
module.exports = {
    Alternative: Alternative, 
    alternativeArray: alternativeArray
};

},{"../Control.Alt":4,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Plus":41,"../Data.Functor":91}],6:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Apply = require("../Control.Apply");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Applicative = function (Apply0, pure) {
    this.Apply0 = Apply0;
    this.pure = pure;
};
var pure = function (dict) {
    return dict.pure;
};
var unless = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (!v) {
                return v1;
            };
            if (v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative line 63, column 1 - line 63, column 19: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var when = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v) {
                return v1;
            };
            if (!v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative line 58, column 1 - line 58, column 16: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var liftA1 = function (dictApplicative) {
    return function (f) {
        return function (a) {
            return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return Control_Apply.applyFn;
}, function (x) {
    return function (v) {
        return x;
    };
});
var applicativeArray = new Applicative(function () {
    return Control_Apply.applyArray;
}, function (x) {
    return [ x ];
});
module.exports = {
    Applicative: Applicative, 
    liftA1: liftA1, 
    pure: pure, 
    unless: unless, 
    when: when, 
    applicativeFn: applicativeFn, 
    applicativeArray: applicativeArray
};

},{"../Control.Apply":8,"../Data.Functor":91,"../Data.Unit":149}],7:[function(require,module,exports){
"use strict";

exports.arrayApply = function (fs) {
  return function (xs) {
    var result = [];
    var n = 0;
    for (var i = 0, l = fs.length; i < l; i++) {
      for (var j = 0, k = xs.length; j < k; j++) {
        result[n++] = fs[i](xs[j]);
      }
    }
    return result;
  };
};

},{}],8:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Category = require("../Control.Category");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Apply = function (Functor0, apply) {
    this.Functor0 = Functor0;
    this.apply = apply;
};
var applyFn = new Apply(function () {
    return Data_Functor.functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var applyArray = new Apply(function () {
    return Data_Functor.functorArray;
}, $foreign.arrayApply);
var apply = function (dict) {
    return dict.apply;
};
var applyFirst = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"])(a))(b);
        };
    };
};
var applySecond = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"](Control_Category.id(Control_Category.categoryFn)))(a))(b);
        };
    };
};
var lift2 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b);
            };
        };
    };
};
var lift3 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c);
                };
            };
        };
    };
};
var lift4 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c))(d);
                    };
                };
            };
        };
    };
};
var lift5 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return apply(dictApply)(apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c))(d))(e);
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    Apply: Apply, 
    apply: apply, 
    applyFirst: applyFirst, 
    applySecond: applySecond, 
    lift2: lift2, 
    lift3: lift3, 
    lift4: lift4, 
    lift5: lift5, 
    applyFn: applyFn, 
    applyArray: applyArray
};

},{"../Control.Category":13,"../Data.Function":88,"../Data.Functor":91,"./foreign":7}],9:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Biapply = require("../Control.Biapply");
var Biapplicative = function (Biapply0, bipure) {
    this.Biapply0 = Biapply0;
    this.bipure = bipure;
};
var bipure = function (dict) {
    return dict.bipure;
};
module.exports = {
    Biapplicative: Biapplicative, 
    bipure: bipure
};

},{"../Control.Biapply":10}],10:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Function = require("../Data.Function");
var Biapply = function (Bifunctor0, biapply) {
    this.Bifunctor0 = Bifunctor0;
    this.biapply = biapply;
};
var biapply = function (dict) {
    return dict.biapply;
};
var biapplyFirst = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(Data_Function["const"](Control_Category.id(Control_Category.categoryFn)))(Data_Function["const"](Control_Category.id(Control_Category.categoryFn))))(a))(b);
        };
    };
};
var biapplySecond = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(Data_Function["const"])(Data_Function["const"]))(a))(b);
        };
    };
};
var bilift2 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(f)(g))(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return biapply(dictBiapply)(biapply(dictBiapply)(Control_Category.id(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(f)(g))(a))(b))(c);
                    };
                };
            };
        };
    };
};
module.exports = {
    Biapply: Biapply, 
    biapply: biapply, 
    biapplyFirst: biapplyFirst, 
    biapplySecond: biapplySecond, 
    bilift2: bilift2, 
    bilift3: bilift3
};

},{"../Control.Category":13,"../Data.Bifunctor":61,"../Data.Function":88}],11:[function(require,module,exports){
"use strict";

exports.arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

},{}],12:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Bind = function (Apply0, bind) {
    this.Apply0 = Apply0;
    this.bind = bind;
};
var Discard = function (discard) {
    this.discard = discard;
};
var discard = function (dict) {
    return dict.discard;
};
var bindFn = new Bind(function () {
    return Control_Apply.applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var bindArray = new Bind(function () {
    return Control_Apply.applyArray;
}, $foreign.arrayBind);
var bind = function (dict) {
    return dict.bind;
};
var bindFlipped = function (dictBind) {
    return Data_Function.flip(bind(dictBind));
};
var composeKleisliFlipped = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bindFlipped(dictBind)(f)(g(a));
            };
        };
    };
};
var composeKleisli = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bind(dictBind)(f(a))(g);
            };
        };
    };
};
var discardUnit = new Discard(function (dictBind) {
    return bind(dictBind);
});
var ifM = function (dictBind) {
    return function (cond) {
        return function (t) {
            return function (f) {
                return bind(dictBind)(cond)(function (cond$prime) {
                    if (cond$prime) {
                        return t;
                    };
                    return f;
                });
            };
        };
    };
};
var join = function (dictBind) {
    return function (m) {
        return bind(dictBind)(m)(Control_Category.id(Control_Category.categoryFn));
    };
};
module.exports = {
    Bind: Bind, 
    Discard: Discard, 
    bind: bind, 
    bindFlipped: bindFlipped, 
    composeKleisli: composeKleisli, 
    composeKleisliFlipped: composeKleisliFlipped, 
    discard: discard, 
    ifM: ifM, 
    join: join, 
    bindFn: bindFn, 
    bindArray: bindArray, 
    discardUnit: discardUnit
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Data.Function":88,"../Data.Functor":91,"../Data.Unit":149,"./foreign":11}],13:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Category = function (Semigroupoid0, id) {
    this.Semigroupoid0 = Semigroupoid0;
    this.id = id;
};
var id = function (dict) {
    return dict.id;
};
var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
}, function (x) {
    return x;
});
module.exports = {
    Category: Category, 
    id: id, 
    categoryFn: categoryFn
};

},{"../Control.Semigroupoid":42}],14:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Extend = require("../Control.Extend");
var Data_Functor = require("../Data.Functor");
var Comonad = function (Extend0, extract) {
    this.Extend0 = Extend0;
    this.extract = extract;
};
var extract = function (dict) {
    return dict.extract;
};
module.exports = {
    Comonad: Comonad, 
    extract: extract
};

},{"../Control.Extend":15,"../Data.Functor":91}],15:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Data_Functor = require("../Data.Functor");
var Data_Semigroup = require("../Data.Semigroup");
var Extend = function (Functor0, extend) {
    this.Functor0 = Functor0;
    this.extend = extend;
};
var extendFn = function (dictSemigroup) {
    return new Extend(function () {
        return Data_Functor.functorFn;
    }, function (f) {
        return function (g) {
            return function (w) {
                return f(function (w$prime) {
                    return g(Data_Semigroup.append(dictSemigroup)(w)(w$prime));
                });
            };
        };
    });
};
var extend = function (dict) {
    return dict.extend;
};
var extendFlipped = function (dictExtend) {
    return function (w) {
        return function (f) {
            return extend(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Control_Category.id(Control_Category.categoryFn));
};
var composeCoKleisliFlipped = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f(extend(dictExtend)(g)(w));
            };
        };
    };
};
var composeCoKleisli = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g(extend(dictExtend)(f)(w));
            };
        };
    };
};
module.exports = {
    Extend: Extend, 
    composeCoKleisli: composeCoKleisli, 
    composeCoKleisliFlipped: composeCoKleisliFlipped, 
    duplicate: duplicate, 
    extend: extend, 
    extendFlipped: extendFlipped, 
    extendFn: extendFn
};

},{"../Control.Category":13,"../Data.Functor":91,"../Data.Semigroup":128}],16:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Unit = require("../Data.Unit");
var Lazy = function (defer) {
    this.defer = defer;
};
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        return defer(dictLazy)(function (v) {
            return f(fix(dictLazy)(f));
        });
    };
};
module.exports = {
    Lazy: Lazy, 
    defer: defer, 
    fix: fix
};

},{"../Data.Unit":149}],17:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Prelude = require("../Prelude");
var MonadCont = function (Monad0, callCC) {
    this.Monad0 = Monad0;
    this.callCC = callCC;
};
var callCC = function (dict) {
    return dict.callCC;
};
module.exports = {
    MonadCont: MonadCont, 
    callCC: callCC
};

},{"../Prelude":171}],18:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var MonadEff = function (Monad0, liftEff) {
    this.Monad0 = Monad0;
    this.liftEff = liftEff;
};
var monadEffEff = new MonadEff(function () {
    return Control_Monad_Eff.monadEff;
}, Control_Category.id(Control_Category.categoryFn));
var liftEff = function (dict) {
    return dict.liftEff;
};
module.exports = {
    MonadEff: MonadEff, 
    liftEff: liftEff, 
    monadEffEff: monadEffEff
};

},{"../Control.Category":13,"../Control.Monad":38,"../Control.Monad.Eff":27}],19:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var unsafeThrowException = function ($0) {
    return Control_Monad_Eff_Unsafe.unsafePerformEff(Control_Monad_Eff_Exception.throwException($0));
};
var unsafeThrow = function ($1) {
    return unsafeThrowException(Control_Monad_Eff_Exception.error($1));
};
module.exports = {
    unsafeThrow: unsafeThrow, 
    unsafeThrowException: unsafeThrowException
};

},{"../Control.Monad.Eff.Exception":21,"../Control.Monad.Eff.Unsafe":25,"../Control.Semigroupoid":42}],20:[function(require,module,exports){
"use strict";

exports.showErrorImpl = function (err) {
  return err.stack || err.toString();
};

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};

exports.stackImpl = function (just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
};

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};

exports.catchException = function (c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
};

},{}],21:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var $$try = function (action) {
    return $foreign.catchException(function ($0) {
        return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Either.Left.create($0));
    })(Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Either.Right.create)(action));
};
var $$throw = function ($1) {
    return $foreign.throwException($foreign.error($1));
};
var stack = $foreign.stackImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var showError = new Data_Show.Show($foreign.showErrorImpl);
module.exports = {
    stack: stack, 
    "throw": $$throw, 
    "try": $$try, 
    showError: showError, 
    catchException: $foreign.catchException, 
    error: $foreign.error, 
    message: $foreign.message, 
    throwException: $foreign.throwException
};

},{"../Control.Applicative":6,"../Control.Monad.Eff":27,"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Show":132,"../Prelude":171,"./foreign":20}],22:[function(require,module,exports){
/* global exports */
"use strict";

exports.setTimeout = function (ms) {
  return function (fn) {
    return function () {
      return setTimeout(fn, ms);
    };
  };
};

exports.clearTimeout = function (id) {
  return function () {
    clearTimeout(id);
  };
};

exports.setInterval = function (ms) {
  return function (fn) {
    return function () {
      return setInterval(fn, ms);
    };
  };
};

exports.clearInterval = function (id) {
  return function () {
    clearInterval(id);
  };
};

},{}],23:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Data_Eq = require("../Data.Eq");
var Data_Ord = require("../Data.Ord");
var Prelude = require("../Prelude");
var TimeoutId = function (x) {
    return x;
};
var IntervalId = function (x) {
    return x;
};
var eqTimeoutId = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordTimeoutId = new Data_Ord.Ord(function () {
    return eqTimeoutId;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
var eqIntervalId = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordIntervalId = new Data_Ord.Ord(function () {
    return eqIntervalId;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
module.exports = {
    eqTimeoutId: eqTimeoutId, 
    ordTimeoutId: ordTimeoutId, 
    eqIntervalId: eqIntervalId, 
    ordIntervalId: ordIntervalId, 
    clearInterval: $foreign.clearInterval, 
    clearTimeout: $foreign.clearTimeout, 
    setInterval: $foreign.setInterval, 
    setTimeout: $foreign.setTimeout
};

},{"../Control.Monad.Eff":27,"../Data.Eq":78,"../Data.Ord":123,"../Prelude":171,"./foreign":22}],24:[function(require,module,exports){
"use strict";

exports.unsafeCoerceEff = function (f) {
  return f;
};

},{}],25:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var unsafePerformEff = function ($0) {
    return Control_Monad_Eff.runPure($foreign.unsafeCoerceEff($0));
};
module.exports = {
    unsafePerformEff: unsafePerformEff, 
    unsafeCoerceEff: $foreign.unsafeCoerceEff
};

},{"../Control.Monad.Eff":27,"../Control.Semigroupoid":42,"./foreign":24}],26:[function(require,module,exports){
"use strict";

exports.pureE = function (a) {
  return function () {
    return a;
  };
};

exports.bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.runPure = function (f) {
  return f();
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

},{}],27:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var monadEff = new Control_Monad.Monad(function () {
    return applicativeEff;
}, function () {
    return bindEff;
});
var bindEff = new Control_Bind.Bind(function () {
    return applyEff;
}, $foreign.bindE);
var applyEff = new Control_Apply.Apply(function () {
    return functorEff;
}, Control_Monad.ap(monadEff));
var applicativeEff = new Control_Applicative.Applicative(function () {
    return applyEff;
}, $foreign.pureE);
var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
module.exports = {
    functorEff: functorEff, 
    applyEff: applyEff, 
    applicativeEff: applicativeEff, 
    bindEff: bindEff, 
    monadEff: monadEff, 
    forE: $foreign.forE, 
    foreachE: $foreign.foreachE, 
    runPure: $foreign.runPure, 
    untilE: $foreign.untilE, 
    whileE: $foreign.whileE
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Monad":38,"../Data.Functor":91,"../Data.Unit":149,"./foreign":26}],28:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var MonadThrow = function (Monad0, throwError) {
    this.Monad0 = Monad0;
    this.throwError = throwError;
};
var MonadError = function (MonadThrow0, catchError) {
    this.MonadThrow0 = MonadThrow0;
    this.catchError = catchError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var monadThrowMaybe = new MonadThrow(function () {
    return Data_Maybe.monadMaybe;
}, Data_Function["const"](Data_Maybe.Nothing.value));
var monadThrowEither = new MonadThrow(function () {
    return Data_Either.monadEither;
}, Data_Either.Left.create);
var monadErrorMaybe = new MonadError(function () {
    return monadThrowMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
            return v1(Data_Unit.unit);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 77, column 3 - line 77, column 33: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var monadErrorEither = new MonadError(function () {
    return monadThrowEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Either.Left) {
            return v1(v.value0);
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 70, column 3 - line 70, column 30: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var catchError = function (dict) {
    return dict.catchError;
};
var catchJust = function (dictMonadError) {
    return function (p) {
        return function (act) {
            return function (handler) {
                var handle = function (e) {
                    var v = p(e);
                    if (v instanceof Data_Maybe.Nothing) {
                        return throwError(dictMonadError.MonadThrow0())(e);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return handler(v.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Error.Class line 54, column 5 - line 56, column 26: " + [ v.constructor.name ]);
                };
                return catchError(dictMonadError)(act)(handle);
            };
        };
    };
};
var $$try = function (dictMonadError) {
    return function (a) {
        return catchError(dictMonadError)(Data_Functor.map(((((dictMonadError.MonadThrow0()).Monad0()).Bind1()).Apply0()).Functor0())(Data_Either.Right.create)(a))(function ($21) {
            return Control_Applicative.pure(((dictMonadError.MonadThrow0()).Monad0()).Applicative0())(Data_Either.Left.create($21));
        });
    };
};
var withResource = function (dictMonadError) {
    return function (acquire) {
        return function (release) {
            return function (kleisli) {
                return Control_Bind.bind(((dictMonadError.MonadThrow0()).Monad0()).Bind1())(acquire)(function (v) {
                    return Control_Bind.bind(((dictMonadError.MonadThrow0()).Monad0()).Bind1())($$try(dictMonadError)(kleisli(v)))(function (v1) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(((dictMonadError.MonadThrow0()).Monad0()).Bind1())(release(v))(function () {
                            return Data_Either.either(throwError(dictMonadError.MonadThrow0()))(Control_Applicative.pure(((dictMonadError.MonadThrow0()).Monad0()).Applicative0()))(v1);
                        });
                    });
                });
            };
        };
    };
};
module.exports = {
    MonadError: MonadError, 
    MonadThrow: MonadThrow, 
    catchError: catchError, 
    catchJust: catchJust, 
    throwError: throwError, 
    "try": $$try, 
    withResource: withResource, 
    monadThrowEither: monadThrowEither, 
    monadErrorEither: monadErrorEither, 
    monadThrowMaybe: monadThrowMaybe, 
    monadErrorMaybe: monadErrorMaybe
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Function":88,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Unit":149,"../Prelude":171}],29:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var ExceptT = function (x) {
    return x;
};
var withExceptT = function (dictFunctor) {
    return function (f) {
        return function (v) {
            var mapLeft = function (v1) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return new Data_Either.Right(v2.value0);
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return new Data_Either.Left(v1(v2.value0));
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 44, column 3 - line 44, column 32: " + [ v1.constructor.name, v2.constructor.name ]);
                };
            };
            return ExceptT(Data_Functor.map(dictFunctor)(mapLeft(f))(v));
        };
    };
};
var runExceptT = function (v) {
    return v;
};
var newtypeExceptT = new Data_Newtype.Newtype(function (n) {
    return n;
}, ExceptT);
var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
        return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v));
        });
    };
});
var mapExceptT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorExceptT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
    });
};
var except = function (dictApplicative) {
    return function ($96) {
        return ExceptT(Control_Applicative.pure(dictApplicative)($96));
    };
};
var monadExceptT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeExceptT(dictMonad);
    }, function () {
        return bindExceptT(dictMonad);
    });
};
var bindExceptT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyExceptT(dictMonad);
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function ($97) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($97));
            })(function (a) {
                var v1 = k(a);
                return v1;
            }));
        };
    });
};
var applyExceptT = function (dictMonad) {
    return new Control_Apply.Apply(function () {
        return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
    }, Control_Monad.ap(monadExceptT(dictMonad)));
};
var applicativeExceptT = function (dictMonad) {
    return new Control_Applicative.Applicative(function () {
        return applyExceptT(dictMonad);
    }, function ($98) {
        return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Right.create($98)));
    });
};
var monadAskExceptT = function (dictMonadAsk) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
        return monadExceptT(dictMonadAsk.Monad0());
    }, Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)));
};
var monadReaderExceptT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadAskExceptT(dictMonadReader.MonadAsk0());
    }, function (f) {
        return mapExceptT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadContExceptT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadExceptT(dictMonadCont.Monad0());
    }, function (f) {
        return ExceptT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            var v = f(function (a) {
                return ExceptT(c(new Data_Either.Right(a)));
            });
            return v;
        }));
    });
};
var monadEffExceptT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadExceptT(dictMonadEff.Monad0());
    }, function ($99) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadEff.Monad0())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($99));
    });
};
var monadRecExceptT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadExceptT(dictMonadRec.Monad0());
    }, function (f) {
        return function ($100) {
            return ExceptT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())((function () {
                    var v = f(a);
                    return v;
                })())(function (m$prime) {
                    return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                        if (m$prime instanceof Data_Either.Left) {
                            return new Control_Monad_Rec_Class.Done(new Data_Either.Left(m$prime.value0));
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
                            return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
                            return new Control_Monad_Rec_Class.Done(new Data_Either.Right(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Except.Trans line 76, column 14 - line 79, column 43: " + [ m$prime.constructor.name ]);
                    })());
                });
            })($100));
        };
    });
};
var monadStateExceptT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadExceptT(dictMonadState.Monad0());
    }, function (f) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadTellExceptT = function (dictMonadTell) {
    return new Control_Monad_Writer_Class.MonadTell(function () {
        return monadExceptT(dictMonadTell.Monad0());
    }, function ($101) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadTell.Monad0())(Control_Monad_Writer_Class.tell(dictMonadTell)($101));
    });
};
var monadWriterExceptT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadTellExceptT(dictMonadWriter.MonadTell0());
    }, mapExceptT(function (m) {
        return Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(Data_Functor.map(Data_Either.functorEither)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapExceptT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(m)(function (v) {
            return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())((function () {
                if (v instanceof Data_Either.Left) {
                    return new Data_Tuple.Tuple(new Data_Either.Left(v.value0), Control_Category.id(Control_Category.categoryFn));
                };
                if (v instanceof Data_Either.Right) {
                    return new Data_Tuple.Tuple(new Data_Either.Right(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 138, column 10 - line 140, column 44: " + [ v.constructor.name ]);
            })());
        }));
    }));
};
var monadThrowExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
        return monadExceptT(dictMonad);
    }, function ($102) {
        return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($102)));
    });
};
var monadErrorExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadThrowExceptT(dictMonad);
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function (a) {
                var v1 = k(a);
                return v1;
            })(function ($103) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Right.create($103));
            }));
        };
    });
};
var altExceptT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Alt.Alt(function () {
            return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
        }, function (v) {
            return function (v1) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v2.value0));
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return Control_Bind.bind(dictMonad.Bind1())(v1)(function (v3) {
                            if (v3 instanceof Data_Either.Right) {
                                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v3.value0));
                            };
                            if (v3 instanceof Data_Either.Left) {
                                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Left(Data_Semigroup.append(dictSemigroup)(v2.value0)(v3.value0)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Except.Trans line 88, column 9 - line 90, column 49: " + [ v3.constructor.name ]);
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 84, column 5 - line 90, column 49: " + [ v2.constructor.name ]);
                });
            };
        });
    };
};
var plusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Plus.Plus(function () {
            return altExceptT(dictMonoid.Semigroup0())(dictMonad);
        }, Control_Monad_Error_Class.throwError(monadThrowExceptT(dictMonad))(Data_Monoid.mempty(dictMonoid)));
    };
};
var alternativeExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Alternative.Alternative(function () {
            return applicativeExceptT(dictMonad);
        }, function () {
            return plusExceptT(dictMonoid)(dictMonad);
        });
    };
};
var monadZeroExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadZero.MonadZero(function () {
            return alternativeExceptT(dictMonoid)(dictMonad);
        }, function () {
            return monadExceptT(dictMonad);
        });
    };
};
var monadPlusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadPlus.MonadPlus(function () {
            return monadZeroExceptT(dictMonoid)(dictMonad);
        });
    };
};
module.exports = {
    ExceptT: ExceptT, 
    except: except, 
    mapExceptT: mapExceptT, 
    runExceptT: runExceptT, 
    withExceptT: withExceptT, 
    newtypeExceptT: newtypeExceptT, 
    functorExceptT: functorExceptT, 
    applyExceptT: applyExceptT, 
    applicativeExceptT: applicativeExceptT, 
    bindExceptT: bindExceptT, 
    monadExceptT: monadExceptT, 
    monadRecExceptT: monadRecExceptT, 
    altExceptT: altExceptT, 
    plusExceptT: plusExceptT, 
    alternativeExceptT: alternativeExceptT, 
    monadPlusExceptT: monadPlusExceptT, 
    monadZeroExceptT: monadZeroExceptT, 
    monadTransExceptT: monadTransExceptT, 
    monadEffExceptT: monadEffExceptT, 
    monadContExceptT: monadContExceptT, 
    monadThrowExceptT: monadThrowExceptT, 
    monadErrorExceptT: monadErrorExceptT, 
    monadAskExceptT: monadAskExceptT, 
    monadReaderExceptT: monadReaderExceptT, 
    monadStateExceptT: monadStateExceptT, 
    monadTellExceptT: monadTellExceptT, 
    monadWriterExceptT: monadWriterExceptT
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Monad":38,"../Control.Monad.Cont.Class":17,"../Control.Monad.Eff.Class":18,"../Control.Monad.Error.Class":28,"../Control.Monad.Reader.Class":31,"../Control.Monad.Rec.Class":32,"../Control.Monad.State.Class":35,"../Control.Monad.Trans.Class":36,"../Control.Monad.Writer.Class":37,"../Control.MonadPlus":39,"../Control.MonadZero":40,"../Control.Plus":41,"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Function":88,"../Data.Functor":91,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Semigroup":128,"../Data.Tuple":145,"../Prelude":171}],30:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Identity = require("../Data.Identity");
var Data_Newtype = require("../Data.Newtype");
var Prelude = require("../Prelude");
var withExcept = Control_Monad_Except_Trans.withExceptT(Data_Identity.functorIdentity);
var runExcept = function ($0) {
    return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(Control_Monad_Except_Trans.runExceptT($0));
};
var mapExcept = function (f) {
    return Control_Monad_Except_Trans.mapExceptT(function ($1) {
        return Data_Identity.Identity(f(Data_Newtype.unwrap(Data_Identity.newtypeIdentity)($1)));
    });
};
module.exports = {
    mapExcept: mapExcept, 
    runExcept: runExcept, 
    withExcept: withExcept
};

},{"../Control.Monad.Error.Class":28,"../Control.Monad.Except.Trans":29,"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Identity":97,"../Data.Newtype":118,"../Prelude":171}],31:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor = require("../Data.Functor");
var Prelude = require("../Prelude");
var MonadAsk = function (Monad0, ask) {
    this.Monad0 = Monad0;
    this.ask = ask;
};
var MonadReader = function (MonadAsk0, local) {
    this.MonadAsk0 = MonadAsk0;
    this.local = local;
};
var monadAskFun = new MonadAsk(function () {
    return Control_Monad.monadFn;
}, Control_Category.id(Control_Category.categoryFn));
var monadReaderFun = new MonadReader(function () {
    return monadAskFun;
}, Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn));
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictMonadAsk) {
    return function (f) {
        return Data_Functor.map((((dictMonadAsk.Monad0()).Bind1()).Apply0()).Functor0())(f)(ask(dictMonadAsk));
    };
};
module.exports = {
    MonadAsk: MonadAsk, 
    MonadReader: MonadReader, 
    ask: ask, 
    asks: asks, 
    local: local, 
    monadAskFun: monadAskFun, 
    monadReaderFun: monadReaderFun
};

},{"../Control.Category":13,"../Control.Monad":38,"../Control.Semigroupoid":42,"../Data.Functor":91,"../Prelude":171}],32:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Unsafe = require("../Control.Monad.Eff.Unsafe");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Either = require("../Data.Either");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Loop = (function () {
    function Loop(value0) {
        this.value0 = value0;
    };
    Loop.create = function (value0) {
        return new Loop(value0);
    };
    return Loop;
})();
var Done = (function () {
    function Done(value0) {
        this.value0 = value0;
    };
    Done.create = function (value0) {
        return new Done(value0);
    };
    return Done;
})();
var MonadRec = function (Monad0, tailRecM) {
    this.Monad0 = Monad0;
    this.tailRecM = tailRecM;
};
var tailRecM = function (dict) {
    return dict.tailRecM;
};
var tailRecM2 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return tailRecM(dictMonadRec)(function (o) {
                    return f(o.a)(o.b);
                })({
                    a: a, 
                    b: b
                });
            };
        };
    };
};
var tailRecM3 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return tailRecM(dictMonadRec)(function (o) {
                        return f(o.a)(o.b)(o.c);
                    })({
                        a: a, 
                        b: b, 
                        c: c
                    });
                };
            };
        };
    };
};
var tailRecEff = function (f) {
    return function (a) {
        var fromDone = function (v) {
            var __unused = function (dictPartial1) {
                return function ($dollar15) {
                    return $dollar15;
                };
            };
            return __unused()((function () {
                if (v instanceof Done) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.Rec.Class line 129, column 28 - line 129, column 42: " + [ v.constructor.name ]);
            })());
        };
        var f$prime = function ($46) {
            return Control_Monad_Eff_Unsafe.unsafeCoerceEff(f($46));
        };
        return function __do() {
            var v = Control_Bind.bindFlipped(Control_Monad_Eff.bindEff)(Control_Monad_ST.newSTRef)(f$prime(a))();
            (function () {
                while (!(function __do() {
                    var v1 = v.value;
                    if (v1 instanceof Loop) {
                        var v2 = f$prime(v1.value0)();
                        var v3 = v.value = v2;
                        return false;
                    };
                    if (v1 instanceof Done) {
                        return true;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Rec.Class line 118, column 5 - line 123, column 26: " + [ v1.constructor.name ]);
                })()) {

                };
                return {};
            })();
            return Data_Functor.map(Control_Monad_Eff.functorEff)(fromDone)(Control_Monad_ST.readSTRef(v))();
        };
    };
};
var tailRec = function (f) {
    var go = function (__copy_v) {
        var __tco_done = false;
        var __tco_result;
        function __tco_loop(v) {
            if (v instanceof Loop) {
                __copy_v = f(v.value0);
                return;
            };
            if (v instanceof Done) {
                __tco_done = true;
                return v.value0;
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class line 93, column 13 - line 96, column 18: " + [ v.constructor.name ]);
        };
        while (!__tco_done) {
            __tco_result = __tco_loop(__copy_v);
        };
        return __tco_result;
    };
    return function ($47) {
        return go(f($47));
    };
};
var monadRecIdentity = new MonadRec(function () {
    return Data_Identity.monadIdentity;
}, function (f) {
    var runIdentity = function (v) {
        return v;
    };
    return function ($48) {
        return Data_Identity.Identity(tailRec(function ($49) {
            return runIdentity(f($49));
        })($48));
    };
});
var monadRecEither = new MonadRec(function () {
    return Data_Either.monadEither;
}, function (f) {
    return function (a0) {
        var g = function (v) {
            if (v instanceof Data_Either.Left) {
                return new Done(new Data_Either.Left(v.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Loop) {
                return new Loop(f(v.value0.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Done) {
                return new Done(new Data_Either.Right(v.value0.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class line 108, column 7 - line 108, column 33: " + [ v.constructor.name ]);
        };
        return tailRec(g)(f(a0));
    };
});
var monadRecEff = new MonadRec(function () {
    return Control_Monad_Eff.monadEff;
}, tailRecEff);
var functorStep = new Data_Functor.Functor(function (f) {
    return function (v) {
        if (v instanceof Loop) {
            return new Loop(v.value0);
        };
        if (v instanceof Done) {
            return new Done(f(v.value0));
        };
        throw new Error("Failed pattern match at Control.Monad.Rec.Class line 28, column 3 - line 28, column 26: " + [ f.constructor.name, v.constructor.name ]);
    };
});
var forever = function (dictMonadRec) {
    return function (ma) {
        return tailRecM(dictMonadRec)(function (u) {
            return Data_Functor.voidRight((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(new Loop(u))(ma);
        })(Data_Unit.unit);
    };
};
var bifunctorStep = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Loop) {
                return new Loop(v(v2.value0));
            };
            if (v2 instanceof Done) {
                return new Done(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class line 32, column 3 - line 32, column 34: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
module.exports = {
    Loop: Loop, 
    Done: Done, 
    MonadRec: MonadRec, 
    forever: forever, 
    tailRec: tailRec, 
    tailRecM: tailRecM, 
    tailRecM2: tailRecM2, 
    tailRecM3: tailRecM3, 
    functorStep: functorStep, 
    bifunctorStep: bifunctorStep, 
    monadRecIdentity: monadRecIdentity, 
    monadRecEff: monadRecEff, 
    monadRecEither: monadRecEither
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Monad.Eff.Unsafe":25,"../Control.Monad.ST":34,"../Control.Semigroupoid":42,"../Data.Bifunctor":61,"../Data.Either":75,"../Data.Functor":91,"../Data.Identity":97,"../Data.Unit":149,"../Partial.Unsafe":168,"../Prelude":171}],33:[function(require,module,exports){
"use strict";

exports.newSTRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readSTRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports.modifySTRef = function (ref) {
  return function (f) {
    return function () {
      return ref.value = f(ref.value); // eslint-disable-line no-return-assign
    };
  };
};

exports.writeSTRef = function (ref) {
  return function (a) {
    return function () {
      return ref.value = a; // eslint-disable-line no-return-assign
    };
  };
};

exports.runST = function (f) {
  return f;
};

},{}],34:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var pureST = function (st) {
    return Control_Monad_Eff.runPure($foreign.runST(st));
};
module.exports = {
    pureST: pureST, 
    modifySTRef: $foreign.modifySTRef, 
    newSTRef: $foreign.newSTRef, 
    readSTRef: $foreign.readSTRef, 
    runST: $foreign.runST, 
    writeSTRef: $foreign.writeSTRef
};

},{"../Control.Monad.Eff":27,"./foreign":33}],35:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var MonadState = function (Monad0, state) {
    this.Monad0 = Monad0;
    this.state = state;
};
var state = function (dict) {
    return dict.state;
};
var put = function (dictMonadState) {
    return function (s) {
        return state(dictMonadState)(function (v) {
            return new Data_Tuple.Tuple(Data_Unit.unit, s);
        });
    };
};
var modify = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
        });
    };
};
var gets = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(f(s), s);
        });
    };
};
var get = function (dictMonadState) {
    return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(s, s);
    });
};
module.exports = {
    MonadState: MonadState, 
    get: get, 
    gets: gets, 
    modify: modify, 
    put: put, 
    state: state
};

},{"../Data.Tuple":145,"../Data.Unit":149,"../Prelude":171}],36:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Prelude = require("../Prelude");
var MonadTrans = function (lift) {
    this.lift = lift;
};
var lift = function (dict) {
    return dict.lift;
};
module.exports = {
    MonadTrans: MonadTrans, 
    lift: lift
};

},{"../Prelude":171}],37:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Data_Function = require("../Data.Function");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var MonadTell = function (Monad0, tell) {
    this.Monad0 = Monad0;
    this.tell = tell;
};
var MonadWriter = function (MonadTell0, listen, pass) {
    this.MonadTell0 = MonadTell0;
    this.listen = listen;
    this.pass = pass;
};
var tell = function (dict) {
    return dict.tell;
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};
var listens = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(listen(dictMonadWriter)(m))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(v.value0, f(v.value1)));
            });
        };
    };
};
var censor = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(m)(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(v, f));
            }));
        };
    };
};
module.exports = {
    MonadTell: MonadTell, 
    MonadWriter: MonadWriter, 
    censor: censor, 
    listen: listen, 
    listens: listens, 
    pass: pass, 
    tell: tell
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Data.Function":88,"../Data.Tuple":145,"../Prelude":171}],38:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var Monad = function (Applicative0, Bind1) {
    this.Applicative0 = Applicative0;
    this.Bind1 = Bind1;
};
var whenM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (v) {
                return Control_Applicative.when(dictMonad.Applicative0())(v)(m);
            });
        };
    };
};
var unlessM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (v) {
                return Control_Applicative.unless(dictMonad.Applicative0())(v)(m);
            });
        };
    };
};
var monadFn = new Monad(function () {
    return Control_Applicative.applicativeFn;
}, function () {
    return Control_Bind.bindFn;
});
var monadArray = new Monad(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Bind.bindArray;
});
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(a)(function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(f(v));
            });
        };
    };
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                });
            });
        };
    };
};
module.exports = {
    Monad: Monad, 
    ap: ap, 
    liftM1: liftM1, 
    unlessM: unlessM, 
    whenM: whenM, 
    monadFn: monadFn, 
    monadArray: monadArray
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Data.Functor":91,"../Data.Unit":149}],39:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var MonadPlus = function (MonadZero0) {
    this.MonadZero0 = MonadZero0;
};
var monadPlusArray = new MonadPlus(function () {
    return Control_MonadZero.monadZeroArray;
});
module.exports = {
    MonadPlus: MonadPlus, 
    monadPlusArray: monadPlusArray
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Monad":38,"../Control.MonadZero":40,"../Control.Plus":41,"../Data.Functor":91}],40:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_Plus = require("../Control.Plus");
var Data_Functor = require("../Data.Functor");
var Data_Unit = require("../Data.Unit");
var MonadZero = function (Alternative1, Monad0) {
    this.Alternative1 = Alternative1;
    this.Monad0 = Monad0;
};
var monadZeroArray = new MonadZero(function () {
    return Control_Alternative.alternativeArray;
}, function () {
    return Control_Monad.monadArray;
});
var guard = function (dictMonadZero) {
    return function (v) {
        if (v) {
            return Control_Applicative.pure((dictMonadZero.Alternative1()).Applicative0())(Data_Unit.unit);
        };
        if (!v) {
            return Control_Plus.empty((dictMonadZero.Alternative1()).Plus1());
        };
        throw new Error("Failed pattern match at Control.MonadZero line 55, column 1 - line 55, column 23: " + [ v.constructor.name ]);
    };
};
module.exports = {
    MonadZero: MonadZero, 
    guard: guard, 
    monadZeroArray: monadZeroArray
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Monad":38,"../Control.Plus":41,"../Data.Functor":91,"../Data.Unit":149}],41:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Data_Functor = require("../Data.Functor");
var Plus = function (Alt0, empty) {
    this.Alt0 = Alt0;
    this.empty = empty;
};
var plusArray = new Plus(function () {
    return Control_Alt.altArray;
}, [  ]);
var empty = function (dict) {
    return dict.empty;
};
module.exports = {
    Plus: Plus, 
    empty: empty, 
    plusArray: plusArray
};

},{"../Control.Alt":4,"../Data.Functor":91}],42:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var compose = function (dict) {
    return dict.compose;
};
var composeFlipped = function (dictSemigroupoid) {
    return function (f) {
        return function (g) {
            return compose(dictSemigroupoid)(g)(f);
        };
    };
};
module.exports = {
    Semigroupoid: Semigroupoid, 
    compose: compose, 
    composeFlipped: composeFlipped, 
    semigroupoidFn: semigroupoidFn
};

},{}],43:[function(require,module,exports){
/* global EventTarget */
"use strict";

exports._readEventTarget = function (left) {
  return function (right) {
    return function (foreign) {
      return foreign instanceof EventTarget ? left("Value is not an EventTarget") : right(foreign);
    };
  };
};

},{}],44:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foreign = require("../Data.Foreign");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var EventType = function (x) {
    return x;
};
var wheelEventToEvent = Unsafe_Coerce.unsafeCoerce;
var uiEventToEvent = Unsafe_Coerce.unsafeCoerce;
var touchEventToEvent = Unsafe_Coerce.unsafeCoerce;
var readWheelEvent = Data_Foreign.unsafeReadTagged("WheelEvent");
var readUIEvent = Data_Foreign.unsafeReadTagged("UIEvent");
var readTouchEvent = Data_Foreign.unsafeReadTagged("TouchEvent");
var readProgressEvent = Data_Foreign.unsafeReadTagged("ProgressEvent");
var readMouseEvent = Data_Foreign.unsafeReadTagged("MouseEvent");
var readKeyboardEvent = Data_Foreign.unsafeReadTagged("KeyboardEvent");
var readInputEvent = Data_Foreign.unsafeReadTagged("InputEvent");
var readFocusEvent = Data_Foreign.unsafeReadTagged("FocusEvent");
var readEventTarget = $foreign._readEventTarget(Data_Either.Left.create)(Data_Either.Right.create);
var readCustomEvent = Data_Foreign.unsafeReadTagged("CustomEvent");
var readCompositionEvent = Data_Foreign.unsafeReadTagged("CompositionEvent");
var readClipboardEvent = Data_Foreign.unsafeReadTagged("ClipboardEvent");
var progressEventToEvent = Unsafe_Coerce.unsafeCoerce;
var ordEventType = Data_Ord.ordString;
var newtypeEventType = new Data_Newtype.Newtype(function (n) {
    return n;
}, EventType);
var mouseEventToEvent = Unsafe_Coerce.unsafeCoerce;
var keyboardEventToEvent = Unsafe_Coerce.unsafeCoerce;
var inputEventToEvent = Unsafe_Coerce.unsafeCoerce;
var focusEventToEvent = Unsafe_Coerce.unsafeCoerce;
var eqEventType = Data_Eq.eqString;
var customEventToEvent = Unsafe_Coerce.unsafeCoerce;
var compositionEventToEvent = Unsafe_Coerce.unsafeCoerce;
var clipboardEventToEvent = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    EventType: EventType, 
    clipboardEventToEvent: clipboardEventToEvent, 
    compositionEventToEvent: compositionEventToEvent, 
    customEventToEvent: customEventToEvent, 
    focusEventToEvent: focusEventToEvent, 
    inputEventToEvent: inputEventToEvent, 
    keyboardEventToEvent: keyboardEventToEvent, 
    mouseEventToEvent: mouseEventToEvent, 
    progressEventToEvent: progressEventToEvent, 
    readClipboardEvent: readClipboardEvent, 
    readCompositionEvent: readCompositionEvent, 
    readCustomEvent: readCustomEvent, 
    readEventTarget: readEventTarget, 
    readFocusEvent: readFocusEvent, 
    readInputEvent: readInputEvent, 
    readKeyboardEvent: readKeyboardEvent, 
    readMouseEvent: readMouseEvent, 
    readProgressEvent: readProgressEvent, 
    readTouchEvent: readTouchEvent, 
    readUIEvent: readUIEvent, 
    readWheelEvent: readWheelEvent, 
    touchEventToEvent: touchEventToEvent, 
    uiEventToEvent: uiEventToEvent, 
    wheelEventToEvent: wheelEventToEvent, 
    newtypeEventType: newtypeEventType, 
    eqEventType: eqEventType, 
    ordEventType: ordEventType
};

},{"../Data.Either":75,"../Data.Eq":78,"../Data.Foreign":85,"../Data.Newtype":118,"../Data.Ord":123,"../Prelude":171,"../Unsafe.Coerce":182,"./foreign":43}],45:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_Util_FFI = require("../DOM.Util.FFI");
var Data_Eq = require("../Data.Eq");
var Data_Foreign = require("../Data.Foreign");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var ElementId = function (x) {
    return x;
};
var textToNode = Unsafe_Coerce.unsafeCoerce;
var readNode = DOM_Util_FFI.unsafeReadProtoTagged("Node");
var readElement = DOM_Util_FFI.unsafeReadProtoTagged("Element");
var readDocument = DOM_Util_FFI.unsafeReadProtoTagged("Document");
var processingInstructionToNode = Unsafe_Coerce.unsafeCoerce;
var oOrdElementId = Data_Ord.ordString;
var newtypeElementId = new Data_Newtype.Newtype(function (n) {
    return n;
}, ElementId);
var eqElementId = Data_Eq.eqString;
var elementToParentNode = Unsafe_Coerce.unsafeCoerce;
var elementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var elementToNode = Unsafe_Coerce.unsafeCoerce;
var elementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentTypeToNode = Unsafe_Coerce.unsafeCoerce;
var documentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNode = Unsafe_Coerce.unsafeCoerce;
var documentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNode = Unsafe_Coerce.unsafeCoerce;
var commentToNode = Unsafe_Coerce.unsafeCoerce;
var characterDataToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    ElementId: ElementId, 
    characterDataToNonDocumentTypeChildNode: characterDataToNonDocumentTypeChildNode, 
    commentToNode: commentToNode, 
    documentFragmentToNode: documentFragmentToNode, 
    documentFragmentToNonElementParentNode: documentFragmentToNonElementParentNode, 
    documentFragmentToParentNode: documentFragmentToParentNode, 
    documentToEventTarget: documentToEventTarget, 
    documentToNode: documentToNode, 
    documentToNonElementParentNode: documentToNonElementParentNode, 
    documentToParentNode: documentToParentNode, 
    documentTypeToNode: documentTypeToNode, 
    elementToEventTarget: elementToEventTarget, 
    elementToNode: elementToNode, 
    elementToNonDocumentTypeChildNode: elementToNonDocumentTypeChildNode, 
    elementToParentNode: elementToParentNode, 
    processingInstructionToNode: processingInstructionToNode, 
    readDocument: readDocument, 
    readElement: readElement, 
    readNode: readNode, 
    textToNode: textToNode, 
    newtypeElementId: newtypeElementId, 
    eqElementId: eqElementId, 
    oOrdElementId: oOrdElementId
};

},{"../DOM.Event.Types":44,"../DOM.Util.FFI":47,"../Data.Eq":78,"../Data.Foreign":85,"../Data.Newtype":118,"../Data.Ord":123,"../Prelude":171,"../Unsafe.Coerce":182}],46:[function(require,module,exports){
"use strict";

exports._unsafeReadProtoTagged = function (name) {
  return function (failure) {
    return function (success) {
      return function (value) {
        var obj = value;
        while (obj != null) {
          var proto = Object.getPrototypeOf(obj);
          var ctor = proto.constructor.name;
          if (ctor === name) {
            return success(value);
          } else if (ctor === "Object") {
            return failure(Object.getPrototypeOf(value).constructor.name);
          }
          obj = proto;
        }
        return failure(Object.getPrototypeOf(value).constructor.name);
      };
    };
  };
};

},{}],47:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Foreign = require("../Data.Foreign");
var Data_Identity = require("../Data.Identity");
var Data_List_Types = require("../Data.List.Types");
var Prelude = require("../Prelude");
var unsafeReadProtoTagged = function (name) {
    return $foreign._unsafeReadProtoTagged(name)(function ($0) {
        return Control_Monad_Except_Trans.except(Data_Identity.applicativeIdentity)(Data_Either.Left.create(Control_Applicative.pure(Data_List_Types.applicativeNonEmptyList)(Data_Foreign.TypeMismatch.create(name)($0))));
    })(function ($1) {
        return Control_Monad_Except_Trans.except(Data_Identity.applicativeIdentity)(Data_Either.Right.create($1));
    });
};
module.exports = {
    unsafeReadProtoTagged: unsafeReadProtoTagged
};

},{"../Control.Applicative":6,"../Control.Monad.Except":30,"../Control.Monad.Except.Trans":29,"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Foreign":85,"../Data.Identity":97,"../Data.List.Types":104,"../Prelude":171,"./foreign":46}],48:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Monad_Eff = require("../Control.Monad.Eff");
module.exports = {};

},{"../Control.Monad.Eff":27}],49:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array_ST = require("../Data.Array.ST");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Semiring = require("../Data.Semiring");
var Prelude = require("../Prelude");
var Iterator = (function () {
    function Iterator(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Iterator.create = function (value0) {
        return function (value1) {
            return new Iterator(value0, value1);
        };
    };
    return Iterator;
})();
var peek = function (v) {
    return function __do() {
        var v1 = Control_Monad_ST.readSTRef(v.value1)();
        return v.value0(v1);
    };
};
var next = function (v) {
    return function __do() {
        var v1 = Control_Monad_ST.readSTRef(v.value1)();
        var v2 = Control_Monad_ST.modifySTRef(v.value1)(function (v2) {
            return v2 + 1 | 0;
        })();
        return v.value0(v1);
    };
};
var pushWhile = function (p) {
    return function (iter) {
        return function (array) {
            return function __do() {
                var v = Control_Monad_ST.newSTRef(false)();
                while (Data_Functor.map(Control_Monad_Eff.functorEff)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST.readSTRef(v))()) {
                    (function __do() {
                        var v1 = peek(iter)();
                        if (v1 instanceof Data_Maybe.Just && p(v1.value0)) {
                            var v2 = Data_Array_ST.pushSTArray(array)(v1.value0)();
                            return Data_Functor["void"](Control_Monad_Eff.functorEff)(next(iter))();
                        };
                        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v)(true))();
                    })();
                };
                return {};
            };
        };
    };
};
var pushAll = pushWhile(Data_Function["const"](true));
var iterator = function (f) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Iterator.create(f))(Control_Monad_ST.newSTRef(0));
};
var iterate = function (iter) {
    return function (f) {
        return function __do() {
            var v = Control_Monad_ST.newSTRef(false)();
            while (Data_Functor.map(Control_Monad_Eff.functorEff)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST.readSTRef(v))()) {
                (function __do() {
                    var v1 = next(iter)();
                    if (v1 instanceof Data_Maybe.Just) {
                        return f(v1.value0)();
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v)(true))();
                    };
                    throw new Error("Failed pattern match at Data.Array.ST.Iterator line 39, column 5 - line 41, column 46: " + [ v1.constructor.name ]);
                })();
            };
            return {};
        };
    };
};
var exhausted = function ($27) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Maybe.isNothing)(peek($27));
};
module.exports = {
    exhausted: exhausted, 
    iterate: iterate, 
    iterator: iterator, 
    next: next, 
    peek: peek, 
    pushAll: pushAll, 
    pushWhile: pushWhile
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Monad.ST":34,"../Control.Semigroupoid":42,"../Data.Array.ST":51,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Semiring":130,"../Prelude":171}],50:[function(require,module,exports){
"use strict";

exports.runSTArray = function (f) {
  return f;
};

exports.emptySTArray = function () {
  return [];
};

exports.peekSTArrayImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return function () {
          return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
        };
      };
    };
  };
};

exports.pokeSTArray = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        var ret = i >= 0 && i < xs.length;
        if (ret) xs[i] = a;
        return ret;
      };
    };
  };
};

exports.pushAllSTArray = function (xs) {
  return function (as) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};

exports.spliceSTArray = function (xs) {
  return function (i) {
    return function (howMany) {
      return function (bs) {
        return function () {
          return xs.splice.apply(xs, [i, howMany].concat(bs));
        };
      };
    };
  };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};

exports.toAssocArray = function (xs) {
  return function () {
    var n = xs.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
    return as;
  };
};

},{}],51:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Maybe = require("../Data.Maybe");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var unsafeFreeze = function ($0) {
    return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Unsafe_Coerce.unsafeCoerce($0));
};
var thaw = $foreign.copyImpl;
var pushSTArray = function (arr) {
    return function (a) {
        return $foreign.pushAllSTArray(arr)([ a ]);
    };
};
var peekSTArray = $foreign.peekSTArrayImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var freeze = $foreign.copyImpl;
module.exports = {
    freeze: freeze, 
    peekSTArray: peekSTArray, 
    pushSTArray: pushSTArray, 
    thaw: thaw, 
    unsafeFreeze: unsafeFreeze, 
    emptySTArray: $foreign.emptySTArray, 
    pokeSTArray: $foreign.pokeSTArray, 
    pushAllSTArray: $foreign.pushAllSTArray, 
    runSTArray: $foreign.runSTArray, 
    spliceSTArray: $foreign.spliceSTArray, 
    toAssocArray: $foreign.toAssocArray
};

},{"../Control.Applicative":6,"../Control.Monad.Eff":27,"../Control.Monad.ST":34,"../Control.Semigroupoid":42,"../Data.Maybe":109,"../Prelude":171,"../Unsafe.Coerce":182,"./foreign":50}],52:[function(require,module,exports){
"use strict";

//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

exports.range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = [];
    var i = start, n = 0;
    while (i !== end) {
      result[n++] = i;
      i += step;
    }
    result[n] = i;
    return result;
  };
};

var replicate = function (count) {
  return function (value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};

var replicatePolyfill = function (count) {
  return function (value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};

// In browsers that have Array.prototype.fill we use it, as it's faster.
exports.replicate = typeof Array.prototype.fill === "function" ?
    replicate :
    replicatePolyfill;

exports.fromFoldableImpl = (function () {
  // jshint maxparams: 2
  function Cons(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  var emptyList = {};

  function curryCons(head) {
    return function (tail) {
      return new Cons(head, tail);
    };
  }

  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }

  return function (foldr) {
    return function (xs) {
      return listToArray(foldr(curryCons)(emptyList)(xs));
    };
  };
})();

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

exports.cons = function (e) {
  return function (l) {
    return [e].concat(l);
  };
};

exports.snoc = function (l) {
  return function (e) {
    var l1 = l.slice();
    l1.push(e);
    return l1;
  };
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

exports["uncons'"] = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

exports.indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

exports.findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports.findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports._insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

exports._deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

exports._updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

exports.reverse = function (l) {
  return l.slice().reverse();
};

exports.concat = function (xss) {
  if (xss.length <= 10000) {
    // This method is faster, but it crashes on big arrays.
    // So we use it when can and fallback to simple variant otherwise.
    return Array.prototype.concat.apply([], xss);
  }

  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

exports.filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

exports.partition = function (f) {
  return function (xs) {
    var yes = [];
    var no  = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes: yes, no: no };
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.sortImpl = function (f) {
  return function (l) {
    // jshint maxparams: 2
    return l.slice().sort(function (x, y) {
      return f(x)(y);
    });
  };
};

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

exports.take = function (n) {
  return function (l) {
    return n < 1 ? [] : l.slice(0, n);
  };
};

exports.drop = function (n) {
  return function (l) {
    return n < 1 ? l : l.slice(n);
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

//------------------------------------------------------------------------------
// Partial ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.unsafeIndexImpl = function (xs) {
  return function (n) {
    return xs[n];
  };
};

},{}],53:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array_ST = require("../Data.Array.ST");
var Data_Array_ST_Iterator = require("../Data.Array.ST.Iterator");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unzip = $foreign["uncons'"](function (v) {
    return new Data_Tuple.Tuple([  ], [  ]);
})(function (v) {
    return function (ts) {
        var v1 = unzip(ts);
        return new Data_Tuple.Tuple($foreign.cons(v.value0)(v1.value0), $foreign.cons(v.value1)(v1.value1));
    };
});
var unsafeIndex = function (dictPartial) {
    return $foreign.unsafeIndexImpl;
};
var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x, 
            tail: xs
        });
    };
});
var toUnfoldable = function (dictUnfoldable) {
    return function (xs) {
        var len = $foreign.length(xs);
        var f = function (i) {
            if (i < len) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(unsafeIndex()(xs)(i), i + 1 | 0));
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Array line 132, column 19 - line 137, column 26: " + [ i.constructor.name ]);
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(f)(0);
    };
};
var tail = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var sortBy = function (comp) {
    return function (xs) {
        var comp$prime = function (x) {
            return function (y) {
                var v = comp(x)(y);
                if (v instanceof Data_Ordering.GT) {
                    return 1;
                };
                if (v instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if (v instanceof Data_Ordering.LT) {
                    return -1 | 0;
                };
                throw new Error("Failed pattern match at Data.Array line 462, column 15 - line 467, column 1: " + [ v.constructor.name ]);
            };
        };
        return $foreign.sortImpl(comp$prime)(xs);
    };
};
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var singleton = function (a) {
    return [ a ];
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubBy = function (eq) {
    return function (xs) {
        var v = uncons(xs);
        if (v instanceof Data_Maybe.Just) {
            return $foreign.cons(v.value0.head)(nubBy(eq)($foreign.filter(function (y) {
                return !eq(v.value0.head)(y);
            })(v.value0.tail)));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match at Data.Array line 570, column 3 - line 572, column 18: " + [ v.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1 | 0))(xs);
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([  ]));
        };
    };
};
var insertAt = $foreign._insertAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1 | 0)(xs));
    };
    throw new Error("Failed pattern match at Data.Array line 248, column 1 - line 250, column 55: " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
};
var unsnoc = function (xs) {
    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return function (v1) {
            return {
                init: v, 
                last: v1
            };
        };
    })(init(xs)))(last(xs));
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
var span = function (p) {
    return function (arr) {
        var go = function (__copy_i) {
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(i) {
                var v = index(arr)(i);
                if (v instanceof Data_Maybe.Just) {
                    var $64 = p(v.value0);
                    if ($64) {
                        __copy_i = i + 1 | 0;
                        return;
                    };
                    __tco_done = true;
                    return new Data_Maybe.Just(i);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    __tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Array line 528, column 5 - line 530, column 25: " + [ v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__copy_i);
            };
            return __tco_result;
        };
        var breakIndex = go(0);
        if (breakIndex instanceof Data_Maybe.Just && breakIndex.value0 === 0) {
            return {
                init: [  ], 
                rest: arr
            };
        };
        if (breakIndex instanceof Data_Maybe.Just) {
            return {
                init: $foreign.slice(0)(breakIndex.value0)(arr), 
                rest: $foreign.slice(breakIndex.value0)($foreign.length(arr))(arr)
            };
        };
        if (breakIndex instanceof Data_Maybe.Nothing) {
            return {
                init: arr, 
                rest: [  ]
            };
        };
        throw new Error("Failed pattern match at Data.Array line 515, column 3 - line 521, column 30: " + [ breakIndex.constructor.name ]);
    };
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var head = function (xs) {
    return index(xs)(0);
};
var groupBy = function (op) {
    return function (xs) {
        return Control_Monad_ST.pureST(function __do() {
            var v = Data_Array_ST.emptySTArray();
            var v1 = Data_Array_ST_Iterator.iterator(function (v1) {
                return index(xs)(v1);
            })();
            Data_Array_ST_Iterator.iterate(v1)(function (x) {
                return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                    var v2 = Data_Array_ST.emptySTArray();
                    Data_Array_ST_Iterator.pushWhile(op(x))(v1)(v2)();
                    var v3 = Data_Array_ST.unsafeFreeze(v2)();
                    return Data_Array_ST.pushSTArray(v)(new Data_NonEmpty.NonEmpty(x, v3))();
                });
            })();
            return Data_Array_ST.unsafeFreeze(v)();
        });
    };
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};
var group$prime = function (dictOrd) {
    return function ($85) {
        return group(dictOrd.Eq0())(sort(dictOrd)($85));
    };
};
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldRecM = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (array) {
                var go = function (res) {
                    return function (i) {
                        if (i >= $foreign.length(array)) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(res));
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(f(res)(unsafeIndex()(array)(i)))(function (v) {
                                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                                    a: v, 
                                    b: i + 1 | 0
                                }));
                            });
                        };
                        throw new Error("Failed pattern match at Data.Array line 671, column 3 - line 675, column 42: " + [ res.constructor.name, i.constructor.name ]);
                    };
                };
                return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(go)(a)(0);
            };
        };
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(a);
            })(function (b) {
                return function (bs) {
                    return Control_Bind.bind(dictMonad.Bind1())(f(a)(b))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)(bs);
                    });
                };
            });
        };
    };
};
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Data_Maybe.fromJust()(insertAt(i)(x)(ys));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Maybe.fromJust()(deleteAt(i)(v2));
            })(findIndex(v(v1))(v2));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldr(Data_Foldable.foldableArray)($$delete(dictEq));
};
var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));
var mapMaybe = function (f) {
    return concatMap(function ($86) {
        return Data_Maybe.maybe([  ])(singleton)(f($86));
    });
};
var filterA = function (dictApplicative) {
    return function (p) {
        return function ($87) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(mapMaybe(function (v) {
                if (v.value1) {
                    return new Data_Maybe.Just(v.value0);
                };
                return Data_Maybe.Nothing.value;
            }))(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(function (x) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(x))(p(x));
            })($87));
        };
    };
};
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var v = f(x);
                if (v instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if (v instanceof Data_Maybe.Just) {
                    return updateAt(i)(v.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array line 388, column 10 - line 390, column 32: " + [ v.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
module.exports = {
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concatMap: concatMap, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filterA: filterA, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    foldRecM: foldRecM, 
    fromFoldable: fromFoldable, 
    group: group, 
    "group'": group$prime, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    last: last, 
    many: many, 
    mapMaybe: mapMaybe, 
    mapWithIndex: mapWithIndex, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    singleton: singleton, 
    some: some, 
    sort: sort, 
    sortBy: sortBy, 
    sortWith: sortWith, 
    span: span, 
    tail: tail, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unsafeIndex: unsafeIndex, 
    unsnoc: unsnoc, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWithA: zipWithA, 
    concat: $foreign.concat, 
    cons: $foreign.cons, 
    drop: $foreign.drop, 
    filter: $foreign.filter, 
    length: $foreign.length, 
    partition: $foreign.partition, 
    range: $foreign.range, 
    replicate: $foreign.replicate, 
    reverse: $foreign.reverse, 
    slice: $foreign.slice, 
    snoc: $foreign.snoc, 
    take: $foreign.take, 
    zipWith: $foreign.zipWith
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Lazy":16,"../Control.Monad.Eff":27,"../Control.Monad.Rec.Class":32,"../Control.Monad.ST":34,"../Control.Semigroupoid":42,"../Data.Array.ST":51,"../Data.Array.ST.Iterator":49,"../Data.Boolean":63,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.NonEmpty":119,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Partial.Unsafe":168,"../Prelude":171,"./foreign":52}],54:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
module.exports = {};

},{}],55:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifunctor_Clown = require("../Data.Bifunctor.Clown");
var Data_Bifunctor_Flip = require("../Data.Bifunctor.Flip");
var Data_Bifunctor_Joker = require("../Data.Bifunctor.Joker");
var Data_Bifunctor_Product = require("../Data.Bifunctor.Product");
var Data_Bifunctor_Wrap = require("../Data.Bifunctor.Wrap");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Bifoldable = function (bifoldMap, bifoldl, bifoldr) {
    this.bifoldMap = bifoldMap;
    this.bifoldl = bifoldl;
    this.bifoldr = bifoldr;
};
var bifoldr = function (dict) {
    return dict.bifoldr;
};
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($97) {
                    return Control_Apply.applySecond(dictApplicative.Apply0())(f($97));
                })(function ($98) {
                    return Control_Apply.applySecond(dictApplicative.Apply0())(g($98));
                })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
            };
        };
    };
};
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldableJoker = function (dictFoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (v) {
            return function (r) {
                return function (v1) {
                    return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(r)(v1);
                };
            };
        };
    }, function (v) {
        return function (r) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldl(dictFoldable)(r)(u)(v1);
                };
            };
        };
    }, function (v) {
        return function (r) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldr(dictFoldable)(r)(u)(v1);
                };
            };
        };
    });
};
var bifoldableClown = function (dictFoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (l) {
            return function (v) {
                return function (v1) {
                    return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(l)(v1);
                };
            };
        };
    }, function (l) {
        return function (v) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldl(dictFoldable)(l)(u)(v1);
                };
            };
        };
    }, function (l) {
        return function (v) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldr(dictFoldable)(l)(u)(v1);
                };
            };
        };
    });
};
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($99) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f($99));
                })(function ($100) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(g($100));
                })(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldl(dictBifoldable)(function (m) {
                    return function (a) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(f(a));
                    };
                })(function (m) {
                    return function (b) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(g(b));
                    };
                })(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldableFlip = function (dictBifoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (r) {
            return function (l) {
                return function (v) {
                    return bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v);
                };
            };
        };
    }, function (r) {
        return function (l) {
            return function (u) {
                return function (v) {
                    return bifoldl(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    }, function (r) {
        return function (l) {
            return function (u) {
                return function (v) {
                    return bifoldr(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    });
};
var bifoldableWrap = function (dictBifoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (l) {
            return function (r) {
                return function (v) {
                    return bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v);
                };
            };
        };
    }, function (l) {
        return function (r) {
            return function (u) {
                return function (v) {
                    return bifoldl(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    }, function (l) {
        return function (r) {
            return function (u) {
                return function (v) {
                    return bifoldr(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    });
};
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap(Data_Monoid_Endo.newtypeEndo)(Data_Newtype.unwrap(Data_Monoid_Dual.newtypeDual)(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($101) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(f)($101)));
                    })(function ($102) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(g)($102)));
                    })(p)))(z);
                };
            };
        };
    };
};
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap(Data_Monoid_Endo.newtypeEndo)(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo)(function ($103) {
                        return Data_Monoid_Endo.Endo(f($103));
                    })(function ($104) {
                        return Data_Monoid_Endo.Endo(g($104));
                    })(p))(z);
                };
            };
        };
    };
};
var bifoldableProduct = function (dictBifoldable) {
    return function (dictBifoldable1) {
        return new Bifoldable(function (dictMonoid) {
            return function (l) {
                return function (r) {
                    return function (v) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v.value0))(bifoldMap(dictBifoldable1)(dictMonoid)(l)(r)(v.value1));
                    };
                };
            };
        }, function (l) {
            return function (r) {
                return function (u) {
                    return function (m) {
                        return bifoldlDefault(bifoldableProduct(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                    };
                };
            };
        }, function (l) {
            return function (r) {
                return function (u) {
                    return function (m) {
                        return bifoldrDefault(bifoldableProduct(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                    };
                };
            };
        });
    };
};
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
    };
};
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($105) {
                    return Data_Newtype.unwrap(Data_Monoid_Disj.newtypeDisj)(bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($106) {
                        return Data_Monoid_Disj.Disj(p($106));
                    })(function ($107) {
                        return Data_Monoid_Disj.Disj(q($107));
                    })($105));
                };
            };
        };
    };
};
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($108) {
                    return Data_Newtype.unwrap(Data_Monoid_Conj.newtypeConj)(bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($109) {
                        return Data_Monoid_Conj.Conj(p($109));
                    })(function ($110) {
                        return Data_Monoid_Conj.Conj(q($110));
                    })($108));
                };
            };
        };
    };
};
module.exports = {
    Bifoldable: Bifoldable, 
    biall: biall, 
    biany: biany, 
    bifold: bifold, 
    bifoldMap: bifoldMap, 
    bifoldMapDefaultL: bifoldMapDefaultL, 
    bifoldMapDefaultR: bifoldMapDefaultR, 
    bifoldl: bifoldl, 
    bifoldlDefault: bifoldlDefault, 
    bifoldr: bifoldr, 
    bifoldrDefault: bifoldrDefault, 
    bifor_: bifor_, 
    bisequence_: bisequence_, 
    bitraverse_: bitraverse_, 
    bifoldableClown: bifoldableClown, 
    bifoldableJoker: bifoldableJoker, 
    bifoldableFlip: bifoldableFlip, 
    bifoldableProduct: bifoldableProduct, 
    bifoldableWrap: bifoldableWrap
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Control.Semigroupoid":42,"../Data.Bifunctor.Clown":56,"../Data.Bifunctor.Flip":57,"../Data.Bifunctor.Joker":58,"../Data.Bifunctor.Product":59,"../Data.Bifunctor.Wrap":60,"../Data.Foldable":83,"../Data.Function":88,"../Data.Monoid":116,"../Data.Monoid.Conj":111,"../Data.Monoid.Disj":112,"../Data.Monoid.Dual":113,"../Data.Monoid.Endo":114,"../Data.Newtype":118,"../Data.Semigroup":128,"../Data.Unit":149,"../Prelude":171}],56:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Clown = function (x) {
    return x;
};
var showClown = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Clown " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordClown = function (dictOrd) {
    return dictOrd;
};
var newtypeClown = new Data_Newtype.Newtype(function (n) {
    return n;
}, Clown);
var functorClown = new Data_Functor.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var eqClown = function (dictEq) {
    return dictEq;
};
var bifunctorClown = function (dictFunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(f)(v1);
            };
        };
    });
};
var biapplyClown = function (dictApply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorClown(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Apply.apply(dictApply)(v)(v1);
        };
    });
};
var biapplicativeClown = function (dictApplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyClown(dictApplicative.Apply0());
    }, function (a) {
        return function (v) {
            return Control_Applicative.pure(dictApplicative)(a);
        };
    });
};
module.exports = {
    Clown: Clown, 
    newtypeClown: newtypeClown, 
    eqClown: eqClown, 
    ordClown: ordClown, 
    showClown: showClown, 
    functorClown: functorClown, 
    bifunctorClown: bifunctorClown, 
    biapplyClown: biapplyClown, 
    biapplicativeClown: biapplicativeClown
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Biapplicative":9,"../Control.Biapply":10,"../Data.Bifunctor":61,"../Data.Eq":78,"../Data.Functor":91,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],57:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Flip = function (x) {
    return x;
};
var showFlip = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Flip " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordFlip = function (dictOrd) {
    return dictOrd;
};
var newtypeFlip = new Data_Newtype.Newtype(function (n) {
    return n;
}, Flip);
var functorFlip = function (dictBifunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return Data_Bifunctor.lmap(dictBifunctor)(f)(v);
        };
    });
};
var eqFlip = function (dictEq) {
    return dictEq;
};
var bifunctorFlip = function (dictBifunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (g) {
            return function (v) {
                return Data_Bifunctor.bimap(dictBifunctor)(g)(f)(v);
            };
        };
    });
};
var biapplyFlip = function (dictBiapply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorFlip(dictBiapply.Bifunctor0());
    }, function (v) {
        return function (v1) {
            return Control_Biapply.biapply(dictBiapply)(v)(v1);
        };
    });
};
var biapplicativeFlip = function (dictBiapplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyFlip(dictBiapplicative.Biapply0());
    }, function (a) {
        return function (b) {
            return Control_Biapplicative.bipure(dictBiapplicative)(b)(a);
        };
    });
};
module.exports = {
    Flip: Flip, 
    newtypeFlip: newtypeFlip, 
    eqFlip: eqFlip, 
    ordFlip: ordFlip, 
    showFlip: showFlip, 
    functorFlip: functorFlip, 
    bifunctorFlip: bifunctorFlip, 
    biapplyFlip: biapplyFlip, 
    biapplicativeFlip: biapplicativeFlip
};

},{"../Control.Biapplicative":9,"../Control.Biapply":10,"../Data.Bifunctor":61,"../Data.Eq":78,"../Data.Functor":91,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],58:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Joker = function (x) {
    return x;
};
var showJoker = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Joker " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordJoker = function (dictOrd) {
    return dictOrd;
};
var newtypeJoker = new Data_Newtype.Newtype(function (n) {
    return n;
}, Joker);
var functorJoker = function (dictFunctor) {
    return new Data_Functor.Functor(function (g) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(g)(v);
        };
    });
};
var eqJoker = function (dictEq) {
    return dictEq;
};
var bifunctorJoker = function (dictFunctor) {
    return new Data_Bifunctor.Bifunctor(function (v) {
        return function (g) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(g)(v1);
            };
        };
    });
};
var biapplyJoker = function (dictApply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorJoker(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Apply.apply(dictApply)(v)(v1);
        };
    });
};
var biapplicativeJoker = function (dictApplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyJoker(dictApplicative.Apply0());
    }, function (v) {
        return function (b) {
            return Control_Applicative.pure(dictApplicative)(b);
        };
    });
};
module.exports = {
    Joker: Joker, 
    newtypeJoker: newtypeJoker, 
    eqJoker: eqJoker, 
    ordJoker: ordJoker, 
    showJoker: showJoker, 
    functorJoker: functorJoker, 
    bifunctorJoker: bifunctorJoker, 
    biapplyJoker: biapplyJoker, 
    biapplicativeJoker: biapplicativeJoker
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Biapplicative":9,"../Control.Biapply":10,"../Data.Bifunctor":61,"../Data.Eq":78,"../Data.Functor":91,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],59:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Product = (function () {
    function Product(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Product.create = function (value0) {
        return function (value1) {
            return new Product(value0, value1);
        };
    };
    return Product;
})();
var showProduct = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(Product " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var eqProduct = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
            };
        });
    };
};
var ordProduct = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqProduct(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
            };
        });
    };
};
var bifunctorProduct = function (dictBifunctor) {
    return function (dictBifunctor1) {
        return new Data_Bifunctor.Bifunctor(function (f) {
            return function (g) {
                return function (v) {
                    return new Product(Data_Bifunctor.bimap(dictBifunctor)(f)(g)(v.value0), Data_Bifunctor.bimap(dictBifunctor1)(f)(g)(v.value1));
                };
            };
        });
    };
};
var biapplyProduct = function (dictBiapply) {
    return function (dictBiapply1) {
        return new Control_Biapply.Biapply(function () {
            return bifunctorProduct(dictBiapply.Bifunctor0())(dictBiapply1.Bifunctor0());
        }, function (v) {
            return function (v1) {
                return new Product(Control_Biapply.biapply(dictBiapply)(v.value0)(v1.value0), Control_Biapply.biapply(dictBiapply1)(v.value1)(v1.value1));
            };
        });
    };
};
var biapplicativeProduct = function (dictBiapplicative) {
    return function (dictBiapplicative1) {
        return new Control_Biapplicative.Biapplicative(function () {
            return biapplyProduct(dictBiapplicative.Biapply0())(dictBiapplicative1.Biapply0());
        }, function (a) {
            return function (b) {
                return new Product(Control_Biapplicative.bipure(dictBiapplicative)(a)(b), Control_Biapplicative.bipure(dictBiapplicative1)(a)(b));
            };
        });
    };
};
module.exports = {
    Product: Product, 
    eqProduct: eqProduct, 
    ordProduct: ordProduct, 
    showProduct: showProduct, 
    bifunctorProduct: bifunctorProduct, 
    biapplyProduct: biapplyProduct, 
    biapplicativeProduct: biapplicativeProduct
};

},{"../Control.Biapplicative":9,"../Control.Biapply":10,"../Data.Bifunctor":61,"../Data.Eq":78,"../Data.HeytingAlgebra":96,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],60:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Wrap = function (x) {
    return x;
};
var showWrap = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Wrap " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordWrap = function (dictOrd) {
    return dictOrd;
};
var newtypeWrap = new Data_Newtype.Newtype(function (n) {
    return n;
}, Wrap);
var functorWrap = function (dictBifunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return Data_Bifunctor.rmap(dictBifunctor)(f)(v);
        };
    });
};
var eqWrap = function (dictEq) {
    return dictEq;
};
var bifunctorWrap = function (dictBifunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (g) {
            return function (v) {
                return Data_Bifunctor.bimap(dictBifunctor)(f)(g)(v);
            };
        };
    });
};
var biapplyWrap = function (dictBiapply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorWrap(dictBiapply.Bifunctor0());
    }, function (v) {
        return function (v1) {
            return Control_Biapply.biapply(dictBiapply)(v)(v1);
        };
    });
};
var biapplicativeWrap = function (dictBiapplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyWrap(dictBiapplicative.Biapply0());
    }, function (a) {
        return function (b) {
            return Control_Biapplicative.bipure(dictBiapplicative)(a)(b);
        };
    });
};
module.exports = {
    Wrap: Wrap, 
    newtypeWrap: newtypeWrap, 
    eqWrap: eqWrap, 
    ordWrap: ordWrap, 
    showWrap: showWrap, 
    functorWrap: functorWrap, 
    bifunctorWrap: bifunctorWrap, 
    biapplyWrap: biapplyWrap, 
    biapplicativeWrap: biapplicativeWrap
};

},{"../Control.Biapplicative":9,"../Control.Biapply":10,"../Data.Bifunctor":61,"../Data.Eq":78,"../Data.Functor":91,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],61:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Bifunctor = function (bimap) {
    this.bimap = bimap;
};
var bimap = function (dict) {
    return dict.bimap;
};
var lmap = function (dictBifunctor) {
    return function (f) {
        return bimap(dictBifunctor)(f)(Control_Category.id(Control_Category.categoryFn));
    };
};
var rmap = function (dictBifunctor) {
    return bimap(dictBifunctor)(Control_Category.id(Control_Category.categoryFn));
};
module.exports = {
    Bifunctor: Bifunctor, 
    bimap: bimap, 
    lmap: lmap, 
    rmap: rmap
};

},{"../Control.Category":13}],62:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bifunctor_Clown = require("../Data.Bifunctor.Clown");
var Data_Bifunctor_Flip = require("../Data.Bifunctor.Flip");
var Data_Bifunctor_Joker = require("../Data.Bifunctor.Joker");
var Data_Bifunctor_Product = require("../Data.Bifunctor.Product");
var Data_Bifunctor_Wrap = require("../Data.Bifunctor.Wrap");
var Data_Functor = require("../Data.Functor");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Bitraversable = function (Bifoldable1, Bifunctor0, bisequence, bitraverse) {
    this.Bifoldable1 = Bifoldable1;
    this.Bifunctor0 = Bifunctor0;
    this.bisequence = bisequence;
    this.bitraverse = bitraverse;
};
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var lfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative))(t);
            };
        };
    };
};
var ltraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative));
        };
    };
};
var rfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative))(f)(t);
            };
        };
    };
};
var rtraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative));
    };
};
var bitraversableJoker = function (dictTraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableJoker(dictTraversable.Foldable1());
    }, function () {
        return Data_Bifunctor_Joker.bifunctorJoker(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Joker.Joker)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (v) {
            return function (r) {
                return function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Joker.Joker)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(r)(v1));
                };
            };
        };
    });
};
var bitraversableClown = function (dictTraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableClown(dictTraversable.Foldable1());
    }, function () {
        return Data_Bifunctor_Clown.bifunctorClown(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Clown.Clown)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (l) {
            return function (v) {
                return function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Clown.Clown)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(l)(v1));
                };
            };
        };
    });
};
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn))(Control_Category.id(Control_Category.categoryFn));
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraversableFlip = function (dictBitraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableFlip(dictBitraversable.Bifoldable1());
    }, function () {
        return Data_Bifunctor_Flip.bifunctorFlip(dictBitraversable.Bifunctor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Flip.Flip)(bisequence(dictBitraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (r) {
            return function (l) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Flip.Flip)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v));
                };
            };
        };
    });
};
var bitraversableProduct = function (dictBitraversable) {
    return function (dictBitraversable1) {
        return new Bitraversable(function () {
            return Data_Bifoldable.bifoldableProduct(dictBitraversable.Bifoldable1())(dictBitraversable1.Bifoldable1());
        }, function () {
            return Data_Bifunctor_Product.bifunctorProduct(dictBitraversable.Bifunctor0())(dictBitraversable1.Bifunctor0());
        }, function (dictApplicative) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Product.Product.create)(bisequence(dictBitraversable)(dictApplicative)(v.value0)))(bisequence(dictBitraversable1)(dictApplicative)(v.value1));
            };
        }, function (dictApplicative) {
            return function (l) {
                return function (r) {
                    return function (v) {
                        return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Product.Product.create)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v.value0)))(bitraverse(dictBitraversable1)(dictApplicative)(l)(r)(v.value1));
                    };
                };
            };
        });
    };
};
var bitraversableWrap = function (dictBitraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableWrap(dictBitraversable.Bifoldable1());
    }, function () {
        return Data_Bifunctor_Wrap.bifunctorWrap(dictBitraversable.Bifunctor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Wrap.Wrap)(bisequence(dictBitraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (l) {
            return function (r) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Wrap.Wrap)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v));
                };
            };
        };
    });
};
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable.Bifunctor0())(f)(g)(t));
                };
            };
        };
    };
};
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
module.exports = {
    Bitraversable: Bitraversable, 
    bifor: bifor, 
    bisequence: bisequence, 
    bisequenceDefault: bisequenceDefault, 
    bitraverse: bitraverse, 
    bitraverseDefault: bitraverseDefault, 
    lfor: lfor, 
    ltraverse: ltraverse, 
    rfor: rfor, 
    rtraverse: rtraverse, 
    bitraversableClown: bitraversableClown, 
    bitraversableJoker: bitraversableJoker, 
    bitraversableFlip: bitraversableFlip, 
    bitraversableProduct: bitraversableProduct, 
    bitraversableWrap: bitraversableWrap
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Data.Bifoldable":55,"../Data.Bifunctor":61,"../Data.Bifunctor.Clown":56,"../Data.Bifunctor.Flip":57,"../Data.Bifunctor.Joker":58,"../Data.Bifunctor.Product":59,"../Data.Bifunctor.Wrap":60,"../Data.Functor":91,"../Data.Traversable":144,"../Prelude":171}],63:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var otherwise = true;
module.exports = {
    otherwise: otherwise
};

},{}],64:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Unit = require("../Data.Unit");
var BooleanAlgebra = function (HeytingAlgebra0) {
    this.HeytingAlgebra0 = HeytingAlgebra0;
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraUnit;
});
var booleanAlgebraFn = function (dictBooleanAlgebra) {
    return new BooleanAlgebra(function () {
        return Data_HeytingAlgebra.heytingAlgebraFunction(dictBooleanAlgebra.HeytingAlgebra0());
    });
};
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraBoolean;
});
module.exports = {
    BooleanAlgebra: BooleanAlgebra, 
    booleanAlgebraBoolean: booleanAlgebraBoolean, 
    booleanAlgebraUnit: booleanAlgebraUnit, 
    booleanAlgebraFn: booleanAlgebraFn
};

},{"../Data.HeytingAlgebra":96,"../Data.Unit":149}],65:[function(require,module,exports){
"use strict";

exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

},{}],66:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Unit = require("../Data.Unit");
var Bounded = function (Ord0, bottom, top) {
    this.Ord0 = Ord0;
    this.bottom = bottom;
    this.top = top;
};
var top = function (dict) {
    return dict.top;
};
var boundedUnit = new Bounded(function () {
    return Data_Ord.ordUnit;
}, Data_Unit.unit, Data_Unit.unit);
var boundedOrdering = new Bounded(function () {
    return Data_Ord.ordOrdering;
}, Data_Ordering.LT.value, Data_Ordering.GT.value);
var boundedInt = new Bounded(function () {
    return Data_Ord.ordInt;
}, $foreign.bottomInt, $foreign.topInt);
var boundedChar = new Bounded(function () {
    return Data_Ord.ordChar;
}, $foreign.bottomChar, $foreign.topChar);
var boundedBoolean = new Bounded(function () {
    return Data_Ord.ordBoolean;
}, false, true);
var bottom = function (dict) {
    return dict.bottom;
};
module.exports = {
    Bounded: Bounded, 
    bottom: bottom, 
    top: top, 
    boundedBoolean: boundedBoolean, 
    boundedInt: boundedInt, 
    boundedChar: boundedChar, 
    boundedOrdering: boundedOrdering, 
    boundedUnit: boundedUnit
};

},{"../Data.Ord":123,"../Data.Ordering":124,"../Data.Unit":149,"./foreign":65}],67:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad = require("../Control.Monad");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_CatQueue = require("../Data.CatQueue");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var CatNil = (function () {
    function CatNil() {

    };
    CatNil.value = new CatNil();
    return CatNil;
})();
var CatCons = (function () {
    function CatCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatCons.create = function (value0) {
        return function (value1) {
            return new CatCons(value0, value1);
        };
    };
    return CatCons;
})();
var showCatList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof CatNil) {
            return "CatNil";
        };
        if (v instanceof CatCons) {
            return "(CatList " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(Data_CatQueue.showCatQueue(showCatList(dictShow)))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Data.CatList line 154, column 3 - line 155, column 3: " + [ v.constructor.name ]);
    });
};
var $$null = function (v) {
    if (v instanceof CatNil) {
        return true;
    };
    return false;
};
var link = function (v) {
    return function (cat) {
        if (v instanceof CatNil) {
            return cat;
        };
        if (v instanceof CatCons) {
            return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(cat));
        };
        throw new Error("Failed pattern match at Data.CatList line 111, column 1 - line 111, column 22: " + [ v.constructor.name, cat.constructor.name ]);
    };
};
var foldr = function (k) {
    return function (b) {
        return function (q) {
            var foldl = function (__copy_v) {
                return function (__copy_c) {
                    return function (__copy_v1) {
                        var __tco_v = __copy_v;
                        var __tco_c = __copy_c;
                        var __tco_done = false;
                        var __tco_result;
                        function __tco_loop(v, c, v1) {
                            if (v1 instanceof Data_List_Types.Nil) {
                                __tco_done = true;
                                return c;
                            };
                            if (v1 instanceof Data_List_Types.Cons) {
                                __tco_v = v;
                                __tco_c = v(c)(v1.value0);
                                __copy_v1 = v1.value1;
                                return;
                            };
                            throw new Error("Failed pattern match at Data.CatList line 126, column 3 - line 126, column 22: " + [ v.constructor.name, c.constructor.name, v1.constructor.name ]);
                        };
                        while (!__tco_done) {
                            __tco_result = __tco_loop(__tco_v, __tco_c, __copy_v1);
                        };
                        return __tco_result;
                    };
                };
            };
            var go = function (__copy_xs) {
                return function (__copy_ys) {
                    var __tco_xs = __copy_xs;
                    var __tco_done = false;
                    var __tco_result;
                    function __tco_loop(xs, ys) {
                        var v = Data_CatQueue.uncons(xs);
                        if (v instanceof Data_Maybe.Nothing) {
                            __tco_done = true;
                            return foldl(function (x) {
                                return function (i) {
                                    return i(x);
                                };
                            })(b)(ys);
                        };
                        if (v instanceof Data_Maybe.Just) {
                            __tco_xs = v.value0.value1;
                            __copy_ys = new Data_List_Types.Cons(k(v.value0.value0), ys);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.CatList line 121, column 14 - line 123, column 67: " + [ v.constructor.name ]);
                    };
                    while (!__tco_done) {
                        __tco_result = __tco_loop(__tco_xs, __copy_ys);
                    };
                    return __tco_result;
                };
            };
            return go(q)(Data_List_Types.Nil.value);
        };
    };
};
var uncons = function (v) {
    if (v instanceof CatNil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof CatCons) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, (function () {
            var $41 = Data_CatQueue["null"](v.value1);
            if ($41) {
                return CatNil.value;
            };
            return foldr(link)(CatNil.value)(v.value1);
        })()));
    };
    throw new Error("Failed pattern match at Data.CatList line 102, column 1 - line 102, column 24: " + [ v.constructor.name ]);
};
var foldMap = function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof CatNil) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof CatCons) {
                var d = (function () {
                    var $46 = Data_CatQueue["null"](v.value1);
                    if ($46) {
                        return CatNil.value;
                    };
                    return foldr(link)(CatNil.value)(v.value1);
                })();
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(foldMap(dictMonoid)(f)(d));
            };
            throw new Error("Failed pattern match at Data.CatList line 142, column 1 - line 142, column 26: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
};
var foldableCatList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (l) {
            return foldMap(dictMonoid)(f)(l);
        };
    };
}, function (f) {
    return function (s) {
        return function (l) {
            return Data_Foldable.foldlDefault(foldableCatList)(f)(s)(l);
        };
    };
}, function (f) {
    return function (s) {
        return function (l) {
            return Data_Foldable.foldrDefault(foldableCatList)(f)(s)(l);
        };
    };
});
var empty = CatNil.value;
var append = function (v) {
    return function (v1) {
        if (v1 instanceof CatNil) {
            return v;
        };
        if (v instanceof CatNil) {
            return v1;
        };
        return link(v)(v1);
    };
};
var cons = function (a) {
    return function (cat) {
        return append(new CatCons(a, Data_CatQueue.empty))(cat);
    };
};
var map = function (v) {
    return function (v1) {
        if (v1 instanceof CatNil) {
            return CatNil.value;
        };
        if (v1 instanceof CatCons) {
            var d = (function () {
                var $53 = Data_CatQueue["null"](v1.value1);
                if ($53) {
                    return CatNil.value;
                };
                return foldr(link)(CatNil.value)(v1.value1);
            })();
            return cons(v(v1.value0))(map(v)(d));
        };
        throw new Error("Failed pattern match at Data.CatList line 136, column 1 - line 136, column 22: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var functorCatList = new Data_Functor.Functor(map);
var singleton = function (a) {
    return cons(a)(CatNil.value);
};
var traversableCatList = new Data_Traversable.Traversable(function () {
    return foldableCatList;
}, function () {
    return functorCatList;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof CatNil) {
            return Control_Applicative.pure(dictApplicative)(CatNil.value);
        };
        if (v instanceof CatCons) {
            var d = (function () {
                var $57 = Data_CatQueue["null"](v.value1);
                if ($57) {
                    return CatNil.value;
                };
                return foldr(link)(CatNil.value)(v.value1);
            })();
            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(cons)(v.value0))(Data_Traversable.sequence(traversableCatList)(dictApplicative)(d));
        };
        throw new Error("Failed pattern match at Data.CatList line 174, column 3 - line 174, column 32: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof CatNil) {
                return Control_Applicative.pure(dictApplicative)(CatNil.value);
            };
            if (v1 instanceof CatCons) {
                var d = (function () {
                    var $62 = Data_CatQueue["null"](v1.value1);
                    if ($62) {
                        return CatNil.value;
                    };
                    return foldr(link)(CatNil.value)(v1.value1);
                })();
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(cons)(v(v1.value0)))(Data_Traversable.traverse(traversableCatList)(dictApplicative)(v)(d));
            };
            throw new Error("Failed pattern match at Data.CatList line 170, column 3 - line 170, column 34: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var semigroupCatList = new Data_Semigroup.Semigroup(append);
var monoidCatList = new Data_Monoid.Monoid(function () {
    return semigroupCatList;
}, CatNil.value);
var monadCatList = new Control_Monad.Monad(function () {
    return applicativeCatList;
}, function () {
    return bindCatList;
});
var bindCatList = new Control_Bind.Bind(function () {
    return applyCatList;
}, Data_Function.flip(foldMap(monoidCatList)));
var applyCatList = new Control_Apply.Apply(function () {
    return functorCatList;
}, Control_Monad.ap(monadCatList));
var applicativeCatList = new Control_Applicative.Applicative(function () {
    return applyCatList;
}, singleton);
var fromFoldable = function (dictFoldable) {
    return function (f) {
        return Data_Foldable.foldMap(dictFoldable)(monoidCatList)(singleton)(f);
    };
};
var snoc = function (cat) {
    return function (a) {
        return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
};
var unfoldableCatList = new Data_Unfoldable.Unfoldable(function (f) {
    return function (b) {
        var go = function (__copy_source) {
            return function (__copy_memo) {
                var __tco_source = __copy_source;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(source, memo) {
                    var v = f(source);
                    if (v instanceof Data_Maybe.Nothing) {
                        __tco_done = true;
                        return memo;
                    };
                    if (v instanceof Data_Maybe.Just) {
                        __tco_source = v.value0.value1;
                        __copy_memo = snoc(memo)(v.value0.value0);
                        return;
                    };
                    throw new Error("Failed pattern match at Data.CatList line 165, column 24 - line 167, column 57: " + [ v.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_source, __copy_memo);
                };
                return __tco_result;
            };
        };
        return go(b)(CatNil.value);
    };
});
var altCatList = new Control_Alt.Alt(function () {
    return functorCatList;
}, append);
var plusCatList = new Control_Plus.Plus(function () {
    return altCatList;
}, empty);
var alternativeCatList = new Control_Alternative.Alternative(function () {
    return applicativeCatList;
}, function () {
    return plusCatList;
});
var monadZeroCatList = new Control_MonadZero.MonadZero(function () {
    return alternativeCatList;
}, function () {
    return monadCatList;
});
var monadPlusCatList = new Control_MonadPlus.MonadPlus(function () {
    return monadZeroCatList;
});
module.exports = {
    CatNil: CatNil, 
    CatCons: CatCons, 
    append: append, 
    cons: cons, 
    empty: empty, 
    fromFoldable: fromFoldable, 
    "null": $$null, 
    snoc: snoc, 
    uncons: uncons, 
    semigroupCatList: semigroupCatList, 
    monoidCatList: monoidCatList, 
    showCatList: showCatList, 
    foldableCatList: foldableCatList, 
    unfoldableCatList: unfoldableCatList, 
    traversableCatList: traversableCatList, 
    functorCatList: functorCatList, 
    applyCatList: applyCatList, 
    applicativeCatList: applicativeCatList, 
    bindCatList: bindCatList, 
    monadCatList: monadCatList, 
    altCatList: altCatList, 
    plusCatList: plusCatList, 
    alternativeCatList: alternativeCatList, 
    monadZeroCatList: monadZeroCatList, 
    monadPlusCatList: monadPlusCatList
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Monad":38,"../Control.MonadPlus":39,"../Control.MonadZero":40,"../Control.Plus":41,"../Data.CatQueue":68,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.NaturalTransformation":117,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unfoldable":147}],68:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var CatQueue = (function () {
    function CatQueue(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatQueue.create = function (value0) {
        return function (value1) {
            return new CatQueue(value0, value1);
        };
    };
    return CatQueue;
})();
var uncons = function (__copy_v) {
    var __tco_done = false;
    var __tco_result;
    function __tco_loop(v) {
        if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
            __tco_done = true;
            return Data_Maybe.Nothing.value;
        };
        if (v.value0 instanceof Data_List_Types.Nil) {
            __copy_v = new CatQueue(Data_List.reverse(v.value1), Data_List_Types.Nil.value);
            return;
        };
        if (v.value0 instanceof Data_List_Types.Cons) {
            __tco_done = true;
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
        };
        throw new Error("Failed pattern match at Data.CatQueue line 51, column 1 - line 51, column 36: " + [ v.constructor.name ]);
    };
    while (!__tco_done) {
        __tco_result = __tco_loop(__copy_v);
    };
    return __tco_result;
};
var snoc = function (v) {
    return function (a) {
        return new CatQueue(v.value0, new Data_List_Types.Cons(a, v.value1));
    };
};
var showCatQueue = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(CatQueue " + (Data_Show.show(Data_List_Types.showList(dictShow))(v.value0) + (" " + (Data_Show.show(Data_List_Types.showList(dictShow))(v.value1) + ")")));
    });
};
var $$null = function (v) {
    if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
        return true;
    };
    return false;
};
var empty = new CatQueue(Data_List_Types.Nil.value, Data_List_Types.Nil.value);
module.exports = {
    CatQueue: CatQueue, 
    empty: empty, 
    "null": $$null, 
    snoc: snoc, 
    uncons: uncons, 
    showCatQueue: showCatQueue
};

},{"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Tuple":145}],69:[function(require,module,exports){
"use strict";

exports.toCharCode = function (c) {
  return c.charCodeAt(0);
};

exports.fromCharCode = function (c) {
  return String.fromCharCode(c);
};

exports.toLower = function (c) {
  return c.toLowerCase();
};

exports.toUpper = function (c) {
  return c.toUpperCase();
};

},{}],70:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    fromCharCode: $foreign.fromCharCode, 
    toCharCode: $foreign.toCharCode, 
    toLower: $foreign.toLower, 
    toUpper: $foreign.toUpper
};

},{"./foreign":69}],71:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var CommutativeRing = function (Ring0) {
    this.Ring0 = Ring0;
};
var commutativeRingUnit = new CommutativeRing(function () {
    return Data_Ring.ringUnit;
});
var commutativeRingNumber = new CommutativeRing(function () {
    return Data_Ring.ringNumber;
});
var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
});
var commutativeRingFn = function (dictCommutativeRing) {
    return new CommutativeRing(function () {
        return Data_Ring.ringFn(dictCommutativeRing.Ring0());
    });
};
module.exports = {
    CommutativeRing: CommutativeRing, 
    commutativeRingInt: commutativeRingInt, 
    commutativeRingNumber: commutativeRingNumber, 
    commutativeRingUnit: commutativeRingUnit, 
    commutativeRingFn: commutativeRingFn
};

},{"../Data.Ring":126,"../Data.Semiring":130,"../Data.Unit":149}],72:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Year = function (x) {
    return x;
};
var Monday = (function () {
    function Monday() {

    };
    Monday.value = new Monday();
    return Monday;
})();
var Tuesday = (function () {
    function Tuesday() {

    };
    Tuesday.value = new Tuesday();
    return Tuesday;
})();
var Wednesday = (function () {
    function Wednesday() {

    };
    Wednesday.value = new Wednesday();
    return Wednesday;
})();
var Thursday = (function () {
    function Thursday() {

    };
    Thursday.value = new Thursday();
    return Thursday;
})();
var Friday = (function () {
    function Friday() {

    };
    Friday.value = new Friday();
    return Friday;
})();
var Saturday = (function () {
    function Saturday() {

    };
    Saturday.value = new Saturday();
    return Saturday;
})();
var Sunday = (function () {
    function Sunday() {

    };
    Sunday.value = new Sunday();
    return Sunday;
})();
var January = (function () {
    function January() {

    };
    January.value = new January();
    return January;
})();
var February = (function () {
    function February() {

    };
    February.value = new February();
    return February;
})();
var March = (function () {
    function March() {

    };
    March.value = new March();
    return March;
})();
var April = (function () {
    function April() {

    };
    April.value = new April();
    return April;
})();
var May = (function () {
    function May() {

    };
    May.value = new May();
    return May;
})();
var June = (function () {
    function June() {

    };
    June.value = new June();
    return June;
})();
var July = (function () {
    function July() {

    };
    July.value = new July();
    return July;
})();
var August = (function () {
    function August() {

    };
    August.value = new August();
    return August;
})();
var September = (function () {
    function September() {

    };
    September.value = new September();
    return September;
})();
var October = (function () {
    function October() {

    };
    October.value = new October();
    return October;
})();
var November = (function () {
    function November() {

    };
    November.value = new November();
    return November;
})();
var December = (function () {
    function December() {

    };
    December.value = new December();
    return December;
})();
var Day = function (x) {
    return x;
};
var showYear = new Data_Show.Show(function (v) {
    return "(Year " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showWeekday = new Data_Show.Show(function (v) {
    if (v instanceof Monday) {
        return "Monday";
    };
    if (v instanceof Tuesday) {
        return "Tuesday";
    };
    if (v instanceof Wednesday) {
        return "Wednesday";
    };
    if (v instanceof Thursday) {
        return "Thursday";
    };
    if (v instanceof Friday) {
        return "Friday";
    };
    if (v instanceof Saturday) {
        return "Saturday";
    };
    if (v instanceof Sunday) {
        return "Sunday";
    };
    throw new Error("Failed pattern match at Data.Date.Component line 190, column 3 - line 191, column 3: " + [ v.constructor.name ]);
});
var showMonth = new Data_Show.Show(function (v) {
    if (v instanceof January) {
        return "January";
    };
    if (v instanceof February) {
        return "February";
    };
    if (v instanceof March) {
        return "March";
    };
    if (v instanceof April) {
        return "April";
    };
    if (v instanceof May) {
        return "May";
    };
    if (v instanceof June) {
        return "June";
    };
    if (v instanceof July) {
        return "July";
    };
    if (v instanceof August) {
        return "August";
    };
    if (v instanceof September) {
        return "September";
    };
    if (v instanceof October) {
        return "October";
    };
    if (v instanceof November) {
        return "November";
    };
    if (v instanceof December) {
        return "December";
    };
    throw new Error("Failed pattern match at Data.Date.Component line 105, column 3 - line 106, column 3: " + [ v.constructor.name ]);
});
var showDay = new Data_Show.Show(function (v) {
    return "(Day " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var ordYear = Data_Ord.ordInt;
var ordDay = Data_Ord.ordInt;
var genericYear = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Year" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Year))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Year", [ {
        sigConstructor: "Data.Date.Component.Year", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Date.Component.Year", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericWeekday = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Monday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Monday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Tuesday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Tuesday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Wednesday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Wednesday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Thursday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Thursday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Friday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Friday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Saturday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Saturday.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Sunday" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Sunday.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Weekday", [ {
        sigConstructor: "Data.Date.Component.Monday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Tuesday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Wednesday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Thursday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Friday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Saturday", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.Sunday", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Monday) {
        return new Data_Generic.SProd("Data.Date.Component.Monday", [  ]);
    };
    if (v instanceof Tuesday) {
        return new Data_Generic.SProd("Data.Date.Component.Tuesday", [  ]);
    };
    if (v instanceof Wednesday) {
        return new Data_Generic.SProd("Data.Date.Component.Wednesday", [  ]);
    };
    if (v instanceof Thursday) {
        return new Data_Generic.SProd("Data.Date.Component.Thursday", [  ]);
    };
    if (v instanceof Friday) {
        return new Data_Generic.SProd("Data.Date.Component.Friday", [  ]);
    };
    if (v instanceof Saturday) {
        return new Data_Generic.SProd("Data.Date.Component.Saturday", [  ]);
    };
    if (v instanceof Sunday) {
        return new Data_Generic.SProd("Data.Date.Component.Sunday", [  ]);
    };
    throw new Error("Failed pattern match at Data.Date.Component line 159, column 1 - line 159, column 50: " + [ v.constructor.name ]);
});
var genericMonth = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.January" && v.value1.length === 0)) {
        return new Data_Maybe.Just(January.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.February" && v.value1.length === 0)) {
        return new Data_Maybe.Just(February.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.March" && v.value1.length === 0)) {
        return new Data_Maybe.Just(March.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.April" && v.value1.length === 0)) {
        return new Data_Maybe.Just(April.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.May" && v.value1.length === 0)) {
        return new Data_Maybe.Just(May.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.June" && v.value1.length === 0)) {
        return new Data_Maybe.Just(June.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.July" && v.value1.length === 0)) {
        return new Data_Maybe.Just(July.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.August" && v.value1.length === 0)) {
        return new Data_Maybe.Just(August.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.September" && v.value1.length === 0)) {
        return new Data_Maybe.Just(September.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.October" && v.value1.length === 0)) {
        return new Data_Maybe.Just(October.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.November" && v.value1.length === 0)) {
        return new Data_Maybe.Just(November.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.December" && v.value1.length === 0)) {
        return new Data_Maybe.Just(December.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Month", [ {
        sigConstructor: "Data.Date.Component.January", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.February", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.March", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.April", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.May", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.June", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.July", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.August", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.September", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.October", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.November", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Date.Component.December", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof January) {
        return new Data_Generic.SProd("Data.Date.Component.January", [  ]);
    };
    if (v instanceof February) {
        return new Data_Generic.SProd("Data.Date.Component.February", [  ]);
    };
    if (v instanceof March) {
        return new Data_Generic.SProd("Data.Date.Component.March", [  ]);
    };
    if (v instanceof April) {
        return new Data_Generic.SProd("Data.Date.Component.April", [  ]);
    };
    if (v instanceof May) {
        return new Data_Generic.SProd("Data.Date.Component.May", [  ]);
    };
    if (v instanceof June) {
        return new Data_Generic.SProd("Data.Date.Component.June", [  ]);
    };
    if (v instanceof July) {
        return new Data_Generic.SProd("Data.Date.Component.July", [  ]);
    };
    if (v instanceof August) {
        return new Data_Generic.SProd("Data.Date.Component.August", [  ]);
    };
    if (v instanceof September) {
        return new Data_Generic.SProd("Data.Date.Component.September", [  ]);
    };
    if (v instanceof October) {
        return new Data_Generic.SProd("Data.Date.Component.October", [  ]);
    };
    if (v instanceof November) {
        return new Data_Generic.SProd("Data.Date.Component.November", [  ]);
    };
    if (v instanceof December) {
        return new Data_Generic.SProd("Data.Date.Component.December", [  ]);
    };
    throw new Error("Failed pattern match at Data.Date.Component line 64, column 1 - line 64, column 46: " + [ v.constructor.name ]);
});
var genericDay = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Component.Day" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Day))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Component.Day", [ {
        sigConstructor: "Data.Date.Component.Day", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Date.Component.Day", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var eqYear = Data_Eq.eqInt;
var eqWeekday = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Monday && y instanceof Monday) {
            return true;
        };
        if (x instanceof Tuesday && y instanceof Tuesday) {
            return true;
        };
        if (x instanceof Wednesday && y instanceof Wednesday) {
            return true;
        };
        if (x instanceof Thursday && y instanceof Thursday) {
            return true;
        };
        if (x instanceof Friday && y instanceof Friday) {
            return true;
        };
        if (x instanceof Saturday && y instanceof Saturday) {
            return true;
        };
        if (x instanceof Sunday && y instanceof Sunday) {
            return true;
        };
        return false;
    };
});
var ordWeekday = new Data_Ord.Ord(function () {
    return eqWeekday;
}, function (x) {
    return function (y) {
        if (x instanceof Monday && y instanceof Monday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Monday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Monday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Tuesday && y instanceof Tuesday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Tuesday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Tuesday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Wednesday && y instanceof Wednesday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Wednesday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Wednesday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Thursday && y instanceof Thursday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Thursday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Thursday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Friday && y instanceof Friday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Friday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Friday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Saturday && y instanceof Saturday) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Saturday) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Saturday) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Sunday && y instanceof Sunday) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component line 158, column 1 - line 158, column 42: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var eqMonth = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof January && y instanceof January) {
            return true;
        };
        if (x instanceof February && y instanceof February) {
            return true;
        };
        if (x instanceof March && y instanceof March) {
            return true;
        };
        if (x instanceof April && y instanceof April) {
            return true;
        };
        if (x instanceof May && y instanceof May) {
            return true;
        };
        if (x instanceof June && y instanceof June) {
            return true;
        };
        if (x instanceof July && y instanceof July) {
            return true;
        };
        if (x instanceof August && y instanceof August) {
            return true;
        };
        if (x instanceof September && y instanceof September) {
            return true;
        };
        if (x instanceof October && y instanceof October) {
            return true;
        };
        if (x instanceof November && y instanceof November) {
            return true;
        };
        if (x instanceof December && y instanceof December) {
            return true;
        };
        return false;
    };
});
var ordMonth = new Data_Ord.Ord(function () {
    return eqMonth;
}, function (x) {
    return function (y) {
        if (x instanceof January && y instanceof January) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof January) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof January) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof February && y instanceof February) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof February) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof February) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof March && y instanceof March) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof March) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof March) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof April && y instanceof April) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof April) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof April) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof May && y instanceof May) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof May) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof May) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof June && y instanceof June) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof June) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof June) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof July && y instanceof July) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof July) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof July) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof August && y instanceof August) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof August) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof August) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof September && y instanceof September) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof September) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof September) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof October && y instanceof October) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof October) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof October) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof November && y instanceof November) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof November) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof November) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof December && y instanceof December) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component line 63, column 1 - line 63, column 38: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var eqDay = Data_Eq.eqInt;
var boundedYear = new Data_Bounded.Bounded(function () {
    return ordYear;
}, -271820 | 0, 275759);
var boundedWeekday = new Data_Bounded.Bounded(function () {
    return ordWeekday;
}, Monday.value, Sunday.value);
var boundedMonth = new Data_Bounded.Bounded(function () {
    return ordMonth;
}, January.value, December.value);
var boundedEnumYear = new Data_Enum.BoundedEnum(function () {
    return boundedYear;
}, function () {
    return enumYear;
}, 547580, function (v) {
    return v;
}, function (n) {
    if (n >= (-271821 | 0) && n <= 275759) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 39, column 3 - line 41, column 26: " + [ n.constructor.name ]);
});
var enumYear = new Data_Enum.Enum(function () {
    return ordYear;
}, function ($110) {
    return Data_Enum.toEnum(boundedEnumYear)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumYear)($110)));
}, function ($111) {
    return Data_Enum.toEnum(boundedEnumYear)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumYear)($111)));
});
var boundedEnumWeekday = new Data_Enum.BoundedEnum(function () {
    return boundedWeekday;
}, function () {
    return enumWeekday;
}, 7, function (v) {
    if (v instanceof Monday) {
        return 1;
    };
    if (v instanceof Tuesday) {
        return 2;
    };
    if (v instanceof Wednesday) {
        return 3;
    };
    if (v instanceof Thursday) {
        return 4;
    };
    if (v instanceof Friday) {
        return 5;
    };
    if (v instanceof Saturday) {
        return 6;
    };
    if (v instanceof Sunday) {
        return 7;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 180, column 14 - line 189, column 1: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(Monday.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(Tuesday.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(Wednesday.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(Thursday.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(Friday.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(Saturday.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(Sunday.value);
    };
    return Data_Maybe.Nothing.value;
});
var enumWeekday = new Data_Enum.Enum(function () {
    return ordWeekday;
}, function ($112) {
    return Data_Enum.toEnum(boundedEnumWeekday)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumWeekday)($112)));
}, function ($113) {
    return Data_Enum.toEnum(boundedEnumWeekday)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumWeekday)($113)));
});
var boundedEnumMonth = new Data_Enum.BoundedEnum(function () {
    return boundedMonth;
}, function () {
    return enumMonth;
}, 12, function (v) {
    if (v instanceof January) {
        return 1;
    };
    if (v instanceof February) {
        return 2;
    };
    if (v instanceof March) {
        return 3;
    };
    if (v instanceof April) {
        return 4;
    };
    if (v instanceof May) {
        return 5;
    };
    if (v instanceof June) {
        return 6;
    };
    if (v instanceof July) {
        return 7;
    };
    if (v instanceof August) {
        return 8;
    };
    if (v instanceof September) {
        return 9;
    };
    if (v instanceof October) {
        return 10;
    };
    if (v instanceof November) {
        return 11;
    };
    if (v instanceof December) {
        return 12;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 90, column 14 - line 104, column 1: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(January.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(February.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(March.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(April.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(May.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(June.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(July.value);
    };
    if (v === 8) {
        return new Data_Maybe.Just(August.value);
    };
    if (v === 9) {
        return new Data_Maybe.Just(September.value);
    };
    if (v === 10) {
        return new Data_Maybe.Just(October.value);
    };
    if (v === 11) {
        return new Data_Maybe.Just(November.value);
    };
    if (v === 12) {
        return new Data_Maybe.Just(December.value);
    };
    return Data_Maybe.Nothing.value;
});
var enumMonth = new Data_Enum.Enum(function () {
    return ordMonth;
}, function ($114) {
    return Data_Enum.toEnum(boundedEnumMonth)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMonth)($114)));
}, function ($115) {
    return Data_Enum.toEnum(boundedEnumMonth)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMonth)($115)));
});
var boundedDay = new Data_Bounded.Bounded(function () {
    return ordDay;
}, 1, 31);
var boundedEnumDay = new Data_Enum.BoundedEnum(function () {
    return boundedDay;
}, function () {
    return enumDay;
}, 31, function (v) {
    return v;
}, function (n) {
    if (n >= 1 && n <= 31) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Date.Component line 139, column 3 - line 141, column 26: " + [ n.constructor.name ]);
});
var enumDay = new Data_Enum.Enum(function () {
    return ordDay;
}, function ($116) {
    return Data_Enum.toEnum(boundedEnumDay)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumDay)($116)));
}, function ($117) {
    return Data_Enum.toEnum(boundedEnumDay)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumDay)($117)));
});
module.exports = {
    January: January, 
    February: February, 
    March: March, 
    April: April, 
    May: May, 
    June: June, 
    July: July, 
    August: August, 
    September: September, 
    October: October, 
    November: November, 
    December: December, 
    Monday: Monday, 
    Tuesday: Tuesday, 
    Wednesday: Wednesday, 
    Thursday: Thursday, 
    Friday: Friday, 
    Saturday: Saturday, 
    Sunday: Sunday, 
    eqYear: eqYear, 
    ordYear: ordYear, 
    genericYear: genericYear, 
    boundedYear: boundedYear, 
    enumYear: enumYear, 
    boundedEnumYear: boundedEnumYear, 
    showYear: showYear, 
    eqMonth: eqMonth, 
    ordMonth: ordMonth, 
    genericMonth: genericMonth, 
    boundedMonth: boundedMonth, 
    enumMonth: enumMonth, 
    boundedEnumMonth: boundedEnumMonth, 
    showMonth: showMonth, 
    eqDay: eqDay, 
    ordDay: ordDay, 
    genericDay: genericDay, 
    boundedDay: boundedDay, 
    enumDay: enumDay, 
    boundedEnumDay: boundedEnumDay, 
    showDay: showDay, 
    eqWeekday: eqWeekday, 
    ordWeekday: ordWeekday, 
    genericWeekday: genericWeekday, 
    boundedWeekday: boundedWeekday, 
    enumWeekday: enumWeekday, 
    boundedEnumWeekday: boundedEnumWeekday, 
    showWeekday: showWeekday
};

},{"../Control.Apply":8,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Bounded":66,"../Data.Enum":76,"../Data.Eq":78,"../Data.Generic":93,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Unit":149,"../Prelude":171}],73:[function(require,module,exports){
"use strict";

exports.canonicalDateImpl = function (ctor, y, m, d) {
  var date = new Date(Date.UTC(y, m - 1, d));
  return ctor(date.getUTCFullYear())(date.getUTCMonth() + 1)(date.getUTCDate());
};

exports.calcWeekday = function (y, m, d) {
  return new Date(Date.UTC(y, m - 1, d)).getUTCDay();
};

exports.calcDiff = function (y1, m1, d1, y2, m2, d2) {
  var dt1 = new Date(Date.UTC(y1, m1 - 1, d1));
  var dt2 = new Date(Date.UTC(y2, m2 - 1, d2));
  return dt1.getTime() - dt2.getTime();
};

},{}],74:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Apply = require("../Control.Apply");
var Data_Bounded = require("../Data.Bounded");
var Data_Date_Component = require("../Data.Date.Component");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Time_Duration = require("../Data.Time.Duration");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var $$Date = (function () {
    function $$Date(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    $$Date.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new $$Date(value0, value1, value2);
            };
        };
    };
    return $$Date;
})();
var year = function (v) {
    return v.value0;
};
var weekday = function (v) {
    var n = $foreign.calcWeekday(v.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v.value1), v.value2);
    var $43 = n === 0;
    if ($43) {
        return Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumWeekday)(7));
    };
    return Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumWeekday)(n));
};
var showDate = new Data_Show.Show(function (v) {
    return "(Date " + (Data_Show.show(Data_Date_Component.showYear)(v.value0) + (" " + (Data_Show.show(Data_Date_Component.showMonth)(v.value1) + (" " + (Data_Show.show(Data_Date_Component.showDay)(v.value2) + ")")))));
});
var month = function (v) {
    return v.value1;
};
var isLeapYear = function (y) {
    var y$prime = Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(y);
    return y$prime % 4 === 0 && (y$prime % 400 === 0 || !(y$prime % 100 === 0));
};
var genericDate = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Date.Date" && v.value1.length === 3)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just($$Date.create))(Data_Generic.fromSpine(Data_Date_Component.genericYear)(v["value1"][0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Date_Component.genericMonth)(v["value1"][1](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Date_Component.genericDay)(v["value1"][2](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Date.Date", [ {
        sigConstructor: "Data.Date.Date", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Date_Component.genericYear)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Date_Component.genericMonth)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Date_Component.genericDay)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Date.Date", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Date_Component.genericYear)(v.value0);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Date_Component.genericMonth)(v.value1);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Date_Component.genericDay)(v.value2);
    } ]);
});
var eqDate = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Date_Component.eqYear)(x.value0)(y.value0) && Data_Eq.eq(Data_Date_Component.eqMonth)(x.value1)(y.value1) && Data_Eq.eq(Data_Date_Component.eqDay)(x.value2)(y.value2);
    };
});
var ordDate = new Data_Ord.Ord(function () {
    return eqDate;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Date_Component.ordYear)(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v1 = Data_Ord.compare(Data_Date_Component.ordMonth)(x.value1)(y.value1);
        if (v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Date_Component.ordDay)(x.value2)(y.value2);
    };
});
var diff = function (dictDuration) {
    return function (v) {
        return function (v1) {
            return Data_Time_Duration.toDuration(dictDuration)($foreign.calcDiff(v.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v.value1), v.value2, v1.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v1.value1), v1.value2));
        };
    };
};
var day = function (v) {
    return v.value2;
};
var canonicalDate = function (y) {
    return function (m) {
        return function (d) {
            var mkDate = function (y$prime) {
                return function (m$prime) {
                    return function (d$prime) {
                        return new $$Date(y$prime, Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(m$prime)), d$prime);
                    };
                };
            };
            return $foreign.canonicalDateImpl(mkDate, y, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(m), d);
        };
    };
};
var exactDate = function (y) {
    return function (m) {
        return function (d) {
            var dt = new $$Date(y, m, d);
            var $95 = Data_Eq.eq(eqDate)(canonicalDate(y)(m)(d))(dt);
            if ($95) {
                return new Data_Maybe.Just(dt);
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var boundedDate = new Data_Bounded.Bounded(function () {
    return ordDate;
}, new $$Date(Data_Bounded.bottom(Data_Date_Component.boundedYear), Data_Bounded.bottom(Data_Date_Component.boundedMonth), Data_Bounded.bottom(Data_Date_Component.boundedDay)), new $$Date(Data_Bounded.top(Data_Date_Component.boundedYear), Data_Bounded.top(Data_Date_Component.boundedMonth), Data_Bounded.top(Data_Date_Component.boundedDay)));
module.exports = {
    canonicalDate: canonicalDate, 
    day: day, 
    diff: diff, 
    exactDate: exactDate, 
    isLeapYear: isLeapYear, 
    month: month, 
    weekday: weekday, 
    year: year, 
    eqDate: eqDate, 
    ordDate: ordDate, 
    genericDate: genericDate, 
    boundedDate: boundedDate, 
    showDate: showDate
};

},{"../Control.Apply":8,"../Data.Bounded":66,"../Data.Date.Component":72,"../Data.Enum":76,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Function":88,"../Data.Function.Uncurried":87,"../Data.Generic":93,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Time.Duration":141,"../Data.Unit":149,"../Partial.Unsafe":168,"../Prelude":171,"./foreign":73}],75:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Left = (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            if (v instanceof Left) {
                return "(Left " + (Data_Show.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Right) {
                return "(Right " + (Data_Show.show(dictShow1)(v.value0) + ")");
            };
            throw new Error("Failed pattern match at Data.Either line 161, column 3 - line 162, column 3: " + [ v.constructor.name ]);
        });
    };
};
var functorEither = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        if (v1 instanceof Right) {
            return new Right(v(v1.value0));
        };
        throw new Error("Failed pattern match at Data.Either line 37, column 3 - line 37, column 26: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invariantEither = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorEither));
var fromRight = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar62) {
                return $dollar62;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Right) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Either line 253, column 1 - line 253, column 23: " + [ v.constructor.name ]);
        })());
    };
};
var fromLeft = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar66) {
                return $dollar66;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Left) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Either line 248, column 1 - line 248, column 22: " + [ v.constructor.name ]);
        })());
    };
};
var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 189, column 3 - line 189, column 31: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 187, column 3 - line 187, column 26: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either line 185, column 3 - line 185, column 26: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableEither = new Data_Traversable.Traversable(function () {
    return foldableEither;
}, function () {
    return functorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Control_Applicative.pure(dictApplicative)(new Left(v.value0));
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 203, column 3 - line 203, column 36: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return Control_Applicative.pure(dictApplicative)(new Left(v1.value0));
            };
            if (v1 instanceof Right) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 201, column 3 - line 201, column 39: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        return new Right(v(v1));
    };
});
var eqEither = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                if (x instanceof Left && y instanceof Left) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0);
                };
                if (x instanceof Right && y instanceof Right) {
                    return Data_Eq.eq(dictEq1)(x.value0)(y.value0);
                };
                return false;
            };
        });
    };
};
var ordEither = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqEither(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                if (x instanceof Left && y instanceof Left) {
                    return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                };
                if (x instanceof Left) {
                    return Data_Ordering.LT.value;
                };
                if (y instanceof Left) {
                    return Data_Ordering.GT.value;
                };
                if (x instanceof Right && y instanceof Right) {
                    return Data_Ord.compare(dictOrd1)(x.value0)(y.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 176, column 1 - line 176, column 64: " + [ x.constructor.name, y.constructor.name ]);
            };
        });
    };
};
var eq1Either = function (dictEq) {
    return new Data_Eq.Eq1(function (dictEq1) {
        return Data_Eq.eq(eqEither(dictEq)(dictEq1));
    });
};
var ord1Either = function (dictOrd) {
    return new Data_Ord.Ord1(function () {
        return eq1Either(dictOrd.Eq0());
    }, function (dictOrd1) {
        return Data_Ord.compare(ordEither(dictOrd)(dictOrd1));
    });
};
var either = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 230, column 1 - line 230, column 26: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isLeft = either(Data_Function["const"](true))(Data_Function["const"](false));
var isRight = either(Data_Function["const"](false))(Data_Function["const"](true));
var choose = function (dictAlt) {
    return function (a) {
        return function (b) {
            return Control_Alt.alt(dictAlt)(Data_Functor.map(dictAlt.Functor0())(Left.create)(a))(Data_Functor.map(dictAlt.Functor0())(Right.create)(b));
        };
    };
};
var boundedEither = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordEither(dictBounded.Ord0())(dictBounded1.Ord0());
        }, new Left(Data_Bounded.bottom(dictBounded)), new Right(Data_Bounded.top(dictBounded1)));
    };
};
var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return new Left(v(v2.value0));
            };
            if (v2 instanceof Right) {
                return new Right(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 44, column 3 - line 44, column 34: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifoldableEither = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 197, column 3 - line 197, column 31: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(z)(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(z)(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 195, column 3 - line 195, column 33: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0)(z);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Either line 193, column 3 - line 193, column 33: " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var bitraversableEither = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableEither;
}, function () {
    return bifunctorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Left.create)(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 209, column 3 - line 209, column 35: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Left.create)(v(v2.value0));
                };
                if (v2 instanceof Right) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v1(v2.value0));
                };
                throw new Error("Failed pattern match at Data.Either line 207, column 3 - line 207, column 41: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var applyEither = new Control_Apply.Apply(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map(functorEither)(v.value0)(v1);
        };
        throw new Error("Failed pattern match at Data.Either line 80, column 3 - line 80, column 28: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindEither = new Control_Bind.Bind(function () {
    return applyEither;
}, either(function (e) {
    return function (v) {
        return new Left(e);
    };
})(function (a) {
    return function (f) {
        return f(a);
    };
}));
var semigroupEither = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semigroup.append(dictSemigroup))(x))(y);
        };
    });
};
var semiringEither = function (dictSemiring) {
    return new Data_Semiring.Semiring(function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semiring.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semiring.mul(dictSemiring))(x))(y);
        };
    }, new Right(Data_Semiring.one(dictSemiring)), new Right(Data_Semiring.zero(dictSemiring)));
};
var applicativeEither = new Control_Applicative.Applicative(function () {
    return applyEither;
}, Right.create);
var monadEither = new Control_Monad.Monad(function () {
    return applicativeEither;
}, function () {
    return bindEither;
});
var altEither = new Control_Alt.Alt(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return v1;
        };
        return v;
    };
});
module.exports = {
    Left: Left, 
    Right: Right, 
    choose: choose, 
    either: either, 
    fromLeft: fromLeft, 
    fromRight: fromRight, 
    isLeft: isLeft, 
    isRight: isRight, 
    functorEither: functorEither, 
    invariantEither: invariantEither, 
    bifunctorEither: bifunctorEither, 
    applyEither: applyEither, 
    applicativeEither: applicativeEither, 
    altEither: altEither, 
    bindEither: bindEither, 
    monadEither: monadEither, 
    extendEither: extendEither, 
    showEither: showEither, 
    eqEither: eqEither, 
    eq1Either: eq1Either, 
    ordEither: ordEither, 
    ord1Either: ord1Either, 
    boundedEither: boundedEither, 
    foldableEither: foldableEither, 
    bifoldableEither: bifoldableEither, 
    traversableEither: traversableEither, 
    bitraversableEither: bitraversableEither, 
    semiringEither: semiringEither, 
    semigroupEither: semigroupEither
};

},{"../Control.Alt":4,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bifoldable":55,"../Data.Bifunctor":61,"../Data.Bitraversable":62,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Traversable":144,"../Prelude":171}],76:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Char = require("../Data.Char");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Cardinality = function (x) {
    return x;
};
var Enum = function (Ord0, pred, succ) {
    this.Ord0 = Ord0;
    this.pred = pred;
    this.succ = succ;
};
var BoundedEnum = function (Bounded0, Enum1, cardinality, fromEnum, toEnum) {
    this.Bounded0 = Bounded0;
    this.Enum1 = Enum1;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.toEnum = toEnum;
};
var toEnum = function (dict) {
    return dict.toEnum;
};
var succ = function (dict) {
    return dict.succ;
};
var pred = function (dict) {
    return dict.pred;
};
var ordCardinality = Data_Ord.ordInt;
var newtypeCardinality = new Data_Newtype.Newtype(function (n) {
    return n;
}, Cardinality);
var intStepFromTo = function (step) {
    return function (from) {
        return function (to) {
            return Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray)(function (e) {
                var $52 = e <= to;
                if ($52) {
                    return Data_Maybe.Just.create(new Data_Tuple.Tuple(e, e + step | 0));
                };
                return Data_Maybe.Nothing.value;
            })(from);
        };
    };
};
var fromEnum = function (dict) {
    return dict.fromEnum;
};
var toEnumWithDefaults = function (dictBoundedEnum) {
    return function (b) {
        return function (t) {
            return function (x) {
                var v = toEnum(dictBoundedEnum)(x);
                if (v instanceof Data_Maybe.Just) {
                    return v.value0;
                };
                if (v instanceof Data_Maybe.Nothing) {
                    var $55 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
                    if ($55) {
                        return b;
                    };
                    return t;
                };
                throw new Error("Failed pattern match at Data.Enum line 251, column 28 - line 253, column 56: " + [ v.constructor.name ]);
            };
        };
    };
};
var eqCardinality = Data_Eq.eqInt;
var enumUnit = new Enum(function () {
    return Data_Ord.ordUnit;
}, Data_Function["const"](Data_Maybe.Nothing.value), Data_Function["const"](Data_Maybe.Nothing.value));
var enumTuple = function (dictEnum) {
    return function (dictBoundedEnum) {
        return new Enum(function () {
            return Data_Tuple.ordTuple(dictEnum.Ord0())((dictBoundedEnum.Enum1()).Ord0());
        }, function (v) {
            return Data_Maybe.maybe(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Bounded.top(dictBoundedEnum.Bounded0())))(pred(dictEnum)(v.value0)))(function ($99) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($99));
            })(pred(dictBoundedEnum.Enum1())(v.value1));
        }, function (v) {
            return Data_Maybe.maybe(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Bounded.bottom(dictBoundedEnum.Bounded0())))(succ(dictEnum)(v.value0)))(function ($100) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($100));
            })(succ(dictBoundedEnum.Enum1())(v.value1));
        });
    };
};
var enumOrdering = new Enum(function () {
    return Data_Ord.ordOrdering;
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_Ordering.EQ) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v instanceof Data_Ordering.GT) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    throw new Error("Failed pattern match at Data.Enum line 74, column 3 - line 74, column 20: " + [ v.constructor.name ]);
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v instanceof Data_Ordering.EQ) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    if (v instanceof Data_Ordering.GT) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Enum line 71, column 3 - line 71, column 20: " + [ v.constructor.name ]);
});
var enumMaybe = function (dictBoundedEnum) {
    return new Enum(function () {
        return Data_Maybe.ordMaybe((dictBoundedEnum.Enum1()).Ord0());
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Maybe.Just.create(pred(dictBoundedEnum.Enum1())(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 81, column 3 - line 81, column 25: " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Just.create(new Data_Maybe.Just(Data_Bounded.bottom(dictBoundedEnum.Bounded0())));
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(succ(dictBoundedEnum.Enum1())(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 79, column 3 - line 79, column 36: " + [ v.constructor.name ]);
    });
};
var enumInt = new Enum(function () {
    return Data_Ord.ordInt;
}, function (n) {
    var $68 = n > Data_Bounded.bottom(Data_Bounded.boundedInt);
    if ($68) {
        return new Data_Maybe.Just(n - 1 | 0);
    };
    return Data_Maybe.Nothing.value;
}, function (n) {
    var $69 = n < Data_Bounded.top(Data_Bounded.boundedInt);
    if ($69) {
        return new Data_Maybe.Just(n + 1 | 0);
    };
    return Data_Maybe.Nothing.value;
});
var enumFromTo = function (dictEnum) {
    return function (dictUnfoldable) {
        return function (from) {
            return function (to) {
                var go = function (mx) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(mx)(function (v) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_MonadZero.guard(Data_Maybe.monadZeroMaybe)(Data_Ord.lessThanOrEq(dictEnum.Ord0())(v)(to)))(function () {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(v, succ(dictEnum)(v)));
                        });
                    });
                };
                return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_Maybe.Just(from));
            };
        };
    };
};
var enumFromThenTo = function (dictBoundedEnum) {
    return function (a) {
        return function (b) {
            return function (c) {
                var c$prime = fromEnum(dictBoundedEnum)(c);
                var b$prime = fromEnum(dictBoundedEnum)(b);
                var a$prime = fromEnum(dictBoundedEnum)(a);
                return Data_Functor.map(Data_Functor.functorArray)(function ($101) {
                    return Data_Maybe.fromJust()(toEnum(dictBoundedEnum)($101));
                })(intStepFromTo(b$prime - a$prime | 0)(a$prime)(c$prime));
            };
        };
    };
};
var enumEither = function (dictBoundedEnum) {
    return function (dictBoundedEnum1) {
        return new Enum(function () {
            return Data_Either.ordEither((dictBoundedEnum.Enum1()).Ord0())((dictBoundedEnum1.Enum1()).Ord0());
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($102) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($102));
                })(pred(dictBoundedEnum.Enum1())(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Left(Data_Bounded.top(dictBoundedEnum.Bounded0()))))(function ($103) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($103));
                })(pred(dictBoundedEnum1.Enum1())(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 87, column 3 - line 87, column 59: " + [ v.constructor.name ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Right(Data_Bounded.bottom(dictBoundedEnum1.Bounded0()))))(function ($104) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($104));
                })(succ(dictBoundedEnum.Enum1())(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($105) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($105));
                })(succ(dictBoundedEnum1.Enum1())(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 85, column 3 - line 85, column 71: " + [ v.constructor.name ]);
        });
    };
};
var enumBoolean = new Enum(function () {
    return Data_Ord.ordBoolean;
}, function (v) {
    if (v) {
        return new Data_Maybe.Just(false);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    if (!v) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
});
var diag = function (a) {
    return new Data_Tuple.Tuple(a, a);
};
var downFrom = function (dictEnum) {
    return function (dictUnfoldable) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)(function ($106) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(diag)(pred(dictEnum)($106));
        });
    };
};
var upFrom = function (dictEnum) {
    return function (dictUnfoldable) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)(function ($107) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(diag)(succ(dictEnum)($107));
        });
    };
};
var defaultToEnum = function (dictBounded) {
    return function (dictEnum) {
        return function (n) {
            if (n < 0) {
                return Data_Maybe.Nothing.value;
            };
            if (n === 0) {
                return new Data_Maybe.Just(Data_Bounded.bottom(dictBounded));
            };
            if (Data_Boolean.otherwise) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(defaultToEnum(dictBounded)(dictEnum)(n - 1 | 0))(succ(dictEnum));
            };
            throw new Error("Failed pattern match at Data.Enum line 231, column 1 - line 234, column 47: " + [ n.constructor.name ]);
        };
    };
};
var defaultSucc = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) + 1 | 0);
        };
    };
};
var defaultPred = function (toEnum$prime) {
    return function (fromEnum$prime) {
        return function (a) {
            return toEnum$prime(fromEnum$prime(a) - 1 | 0);
        };
    };
};
var defaultFromEnum = function (dictEnum) {
    return function ($108) {
        return Data_Maybe.maybe(0)(function (prd) {
            return defaultFromEnum(dictEnum)(prd) + 1 | 0;
        })(pred(dictEnum)($108));
    };
};
var defaultCardinality = function (dictBounded) {
    return function (dictEnum) {
        var defaultCardinality$prime = function (i) {
            return function ($109) {
                return Data_Maybe.maybe(i)(defaultCardinality$prime(i + 1 | 0))(succ(dictEnum)($109));
            };
        };
        return Cardinality(defaultCardinality$prime(1)(Data_Bounded.bottom(dictBounded)));
    };
};
var charToEnum = function (v) {
    if (v >= Data_Bounded.bottom(Data_Bounded.boundedInt) && v <= Data_Bounded.top(Data_Bounded.boundedInt)) {
        return Data_Maybe.Just.create(Data_Char.fromCharCode(v));
    };
    return Data_Maybe.Nothing.value;
};
var enumChar = new Enum(function () {
    return Data_Ord.ordChar;
}, defaultPred(charToEnum)(Data_Char.toCharCode), defaultSucc(charToEnum)(Data_Char.toCharCode));
var cardinality = function (dict) {
    return dict.cardinality;
};
var boundedEnumUnit = new BoundedEnum(function () {
    return Data_Bounded.boundedUnit;
}, function () {
    return enumUnit;
}, 1, Data_Function["const"](0), function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Data_Unit.unit);
    };
    return Data_Maybe.Nothing.value;
});
var boundedEnumTuple = function (dictBoundedEnum) {
    return function (dictBoundedEnum1) {
        return new BoundedEnum(function () {
            return Data_Tuple.boundedTuple(dictBoundedEnum.Bounded0())(dictBoundedEnum1.Bounded0());
        }, function () {
            return enumTuple(dictBoundedEnum.Enum1())(dictBoundedEnum1);
        }, Cardinality(Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum)) * Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum1)) | 0), (function () {
            var from = function (v) {
                return function (v1) {
                    return (fromEnum(dictBoundedEnum)(v1.value0) * v | 0) + fromEnum(dictBoundedEnum1)(v1.value1) | 0;
                };
            };
            return from(cardinality(dictBoundedEnum1));
        })(), (function () {
            var to = function (v) {
                return function (n) {
                    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(toEnum(dictBoundedEnum)(n / v | 0)))(toEnum(dictBoundedEnum1)(n % v));
                };
            };
            return to(cardinality(dictBoundedEnum1));
        })());
    };
};
var boundedEnumOrdering = new BoundedEnum(function () {
    return Data_Bounded.boundedOrdering;
}, function () {
    return enumOrdering;
}, 3, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return 0;
    };
    if (v instanceof Data_Ordering.EQ) {
        return 1;
    };
    if (v instanceof Data_Ordering.GT) {
        return 2;
    };
    throw new Error("Failed pattern match at Data.Enum line 185, column 3 - line 186, column 3: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    return Data_Maybe.Nothing.value;
});
var boundedEnumMaybe = function (dictBoundedEnum) {
    return new BoundedEnum(function () {
        return Data_Maybe.boundedMaybe(dictBoundedEnum.Bounded0());
    }, function () {
        return enumMaybe(dictBoundedEnum);
    }, Cardinality(Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum)) + 1 | 0), function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return 0;
        };
        if (v instanceof Data_Maybe.Just) {
            return fromEnum(dictBoundedEnum)(v.value0) + 1 | 0;
        };
        throw new Error("Failed pattern match at Data.Enum line 193, column 3 - line 194, column 3: " + [ v.constructor.name ]);
    }, function (v) {
        if (v === 0) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
        };
        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(toEnum(dictBoundedEnum)(v - 1 | 0));
    });
};
var boundedEnumEither = function (dictBoundedEnum) {
    return function (dictBoundedEnum1) {
        return new BoundedEnum(function () {
            return Data_Either.boundedEither(dictBoundedEnum.Bounded0())(dictBoundedEnum1.Bounded0());
        }, function () {
            return enumEither(dictBoundedEnum)(dictBoundedEnum1);
        }, Cardinality(Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum)) + Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum1)) | 0), function (v) {
            if (v instanceof Data_Either.Left) {
                return fromEnum(dictBoundedEnum)(v.value0);
            };
            if (v instanceof Data_Either.Right) {
                return fromEnum(dictBoundedEnum1)(v.value0) + Data_Newtype.unwrap(newtypeCardinality)(cardinality(dictBoundedEnum)) | 0;
            };
            throw new Error("Failed pattern match at Data.Enum line 207, column 3 - line 207, column 33: " + [ v.constructor.name ]);
        }, function (n) {
            var to = function (v) {
                if (n >= 0 && n < v) {
                    return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Left.create)(toEnum(dictBoundedEnum)(n));
                };
                if (Data_Boolean.otherwise) {
                    return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Right.create)(toEnum(dictBoundedEnum1)(n - v | 0));
                };
                throw new Error("Failed pattern match at Data.Enum line 204, column 5 - line 206, column 46: " + [ v.constructor.name ]);
            };
            return to(cardinality(dictBoundedEnum));
        });
    };
};
var boundedEnumChar = new BoundedEnum(function () {
    return Data_Bounded.boundedChar;
}, function () {
    return enumChar;
}, Data_Char.toCharCode(Data_Bounded.top(Data_Bounded.boundedChar)) - Data_Char.toCharCode(Data_Bounded.bottom(Data_Bounded.boundedChar)) | 0, Data_Char.toCharCode, charToEnum);
var boundedEnumBoolean = new BoundedEnum(function () {
    return Data_Bounded.boundedBoolean;
}, function () {
    return enumBoolean;
}, 2, function (v) {
    if (!v) {
        return 0;
    };
    if (v) {
        return 1;
    };
    throw new Error("Failed pattern match at Data.Enum line 165, column 3 - line 166, column 3: " + [ v.constructor.name ]);
}, function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(false);
    };
    if (v === 1) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
});
module.exports = {
    Cardinality: Cardinality, 
    BoundedEnum: BoundedEnum, 
    Enum: Enum, 
    cardinality: cardinality, 
    defaultCardinality: defaultCardinality, 
    defaultFromEnum: defaultFromEnum, 
    defaultPred: defaultPred, 
    defaultSucc: defaultSucc, 
    defaultToEnum: defaultToEnum, 
    downFrom: downFrom, 
    enumFromThenTo: enumFromThenTo, 
    enumFromTo: enumFromTo, 
    fromEnum: fromEnum, 
    pred: pred, 
    succ: succ, 
    toEnum: toEnum, 
    toEnumWithDefaults: toEnumWithDefaults, 
    upFrom: upFrom, 
    newtypeCardinality: newtypeCardinality, 
    eqCardinality: eqCardinality, 
    ordCardinality: ordCardinality, 
    enumBoolean: enumBoolean, 
    enumInt: enumInt, 
    enumChar: enumChar, 
    enumUnit: enumUnit, 
    enumOrdering: enumOrdering, 
    enumMaybe: enumMaybe, 
    enumEither: enumEither, 
    enumTuple: enumTuple, 
    boundedEnumBoolean: boundedEnumBoolean, 
    boundedEnumChar: boundedEnumChar, 
    boundedEnumUnit: boundedEnumUnit, 
    boundedEnumOrdering: boundedEnumOrdering, 
    boundedEnumMaybe: boundedEnumMaybe, 
    boundedEnumEither: boundedEnumEither, 
    boundedEnumTuple: boundedEnumTuple
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.MonadPlus":39,"../Control.MonadZero":40,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Bounded":66,"../Data.Char":70,"../Data.Either":75,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semiring":130,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Data.Unit":149,"../Partial.Unsafe":168,"../Prelude":171}],77:[function(require,module,exports){
"use strict";

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.refIneq = function (r1) {
  return function (r2) {
    return r1 !== r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

},{}],78:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Eq = function (eq) {
    this.eq = eq;
};
var Eq1 = function (eq1) {
    this.eq1 = eq1;
};
var eqVoid = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqUnit = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqString = new Eq($foreign.refEq);
var eqNumber = new Eq($foreign.refEq);
var eqInt = new Eq($foreign.refEq);
var eqChar = new Eq($foreign.refEq);
var eqBoolean = new Eq($foreign.refEq);
var eq1 = function (dict) {
    return dict.eq1;
};
var eq = function (dict) {
    return dict.eq;
};
var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl(eq(dictEq)));
};
var eq1Array = new Eq1(function (dictEq) {
    return eq(eqArray(dictEq));
});
var notEq = function (dictEq) {
    return function (x) {
        return function (y) {
            return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
        };
    };
};
var notEq1 = function (dictEq1) {
    return function (dictEq) {
        return function (x) {
            return function (y) {
                return eq(eqBoolean)(eq1(dictEq1)(dictEq)(x)(y))(false);
            };
        };
    };
};
module.exports = {
    Eq: Eq, 
    Eq1: Eq1, 
    eq: eq, 
    eq1: eq1, 
    notEq: notEq, 
    notEq1: notEq1, 
    eqBoolean: eqBoolean, 
    eqInt: eqInt, 
    eqNumber: eqNumber, 
    eqChar: eqChar, 
    eqString: eqString, 
    eqUnit: eqUnit, 
    eqVoid: eqVoid, 
    eqArray: eqArray, 
    eq1Array: eq1Array
};

},{"../Data.Unit":149,"../Data.Void":150,"./foreign":77}],79:[function(require,module,exports){
"use strict";

exports.intDegree = function (x) {
  return Math.min(Math.abs(x), 2147483647);
};

exports.intDiv = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.intMod = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

},{}],80:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var EuclideanRing = function (CommutativeRing0, degree, div, mod) {
    this.CommutativeRing0 = CommutativeRing0;
    this.degree = degree;
    this.div = div;
    this.mod = mod;
};
var mod = function (dict) {
    return dict.mod;
};
var gcd = function (__copy_dictEq) {
    return function (__copy_dictEuclideanRing) {
        return function (__copy_a) {
            return function (__copy_b) {
                var __tco_dictEq = __copy_dictEq;
                var __tco_dictEuclideanRing = __copy_dictEuclideanRing;
                var __tco_a = __copy_a;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(dictEq, dictEuclideanRing, a, b) {
                    var $7 = Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    if ($7) {
                        __tco_done = true;
                        return a;
                    };
                    __tco_dictEq = dictEq;
                    __tco_dictEuclideanRing = dictEuclideanRing;
                    __tco_a = b;
                    __copy_b = mod(dictEuclideanRing)(a)(b);
                    return;
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_dictEq, __tco_dictEuclideanRing, __tco_a, __copy_b);
                };
                return __tco_result;
            };
        };
    };
};
var euclideanRingNumber = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingNumber;
}, function (v) {
    return 1;
}, $foreign.numDiv, function (v) {
    return function (v1) {
        return 0.0;
    };
});
var euclideanRingInt = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingInt;
}, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
var div = function (dict) {
    return dict.div;
};
var lcm = function (dictEq) {
    return function (dictEuclideanRing) {
        return function (a) {
            return function (b) {
                var $8 = Data_Eq.eq(dictEq)(a)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())) || Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                if ($8) {
                    return Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0());
                };
                return div(dictEuclideanRing)(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(a)(b))(gcd(dictEq)(dictEuclideanRing)(a)(b));
            };
        };
    };
};
var degree = function (dict) {
    return dict.degree;
};
module.exports = {
    EuclideanRing: EuclideanRing, 
    degree: degree, 
    div: div, 
    gcd: gcd, 
    lcm: lcm, 
    mod: mod, 
    euclideanRingInt: euclideanRingInt, 
    euclideanRingNumber: euclideanRingNumber
};

},{"../Data.BooleanAlgebra":64,"../Data.CommutativeRing":71,"../Data.Eq":78,"../Data.HeytingAlgebra":96,"../Data.Ring":126,"../Data.Semiring":130,"./foreign":79}],81:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Field = function (EuclideanRing0) {
    this.EuclideanRing0 = EuclideanRing0;
};
var fieldNumber = new Field(function () {
    return Data_EuclideanRing.euclideanRingNumber;
});
module.exports = {
    Field: Field, 
    fieldNumber: fieldNumber
};

},{"../Data.CommutativeRing":71,"../Data.EuclideanRing":80,"../Data.Ring":126,"../Data.Semiring":130}],82:[function(require,module,exports){
"use strict";

exports.foldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.foldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

},{}],83:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Plus = require("../Control.Plus");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Endo = require("../Data.Monoid.Endo");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Foldable = function (foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
};
var foldr = function (dict) {
    return dict.foldr;
};
var $$null = function (dictFoldable) {
    return foldr(dictFoldable)(function (v) {
        return function (v1) {
            return false;
        };
    })(true);
};
var oneOf = function (dictFoldable) {
    return function (dictPlus) {
        return foldr(dictFoldable)(Control_Alt.alt(dictPlus.Alt0()))(Control_Plus.empty(dictPlus));
    };
};
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)(function ($175) {
                return Control_Apply.applySecond(dictApplicative.Apply0())(f($175));
            })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
};
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Control_Category.id(Control_Category.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false, 
                                acc: x
                            };
                        };
                        return {
                            init: false, 
                            acc: Data_Semigroup.append(dictMonoid.Semigroup0())(v.acc)(Data_Semigroup.append(dictMonoid.Semigroup0())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true, 
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};
var length = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(function (c) {
            return function (v) {
                return Data_Semiring.add(dictSemiring)(Data_Semiring.one(dictSemiring))(c);
            };
        })(Data_Semiring.zero(dictSemiring));
    };
};
var maximumBy = function (dictFoldable) {
    return function (cmp) {
        var max$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $98 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.GT.value);
                        if ($98) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 296, column 3 - line 296, column 27: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(max$prime)(Data_Maybe.Nothing.value);
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable) {
        return maximumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var minimumBy = function (dictFoldable) {
    return function (cmp) {
        var min$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $102 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.LT.value);
                        if ($102) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable line 309, column 3 - line 309, column 27: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(min$prime)(Data_Maybe.Nothing.value);
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable) {
        return minimumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
    };
};
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
    };
};
var foldableMultiplicative = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 130, column 3 - line 130, column 30: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 128, column 3 - line 128, column 25: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Foldable line 126, column 3 - line 126, column 25: " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var foldableDual = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableDisj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableConj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableAdditive = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldr(dictFoldable)(function (x) {
                return function (acc) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
}, $foreign.foldlArray, $foreign.foldrArray);
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldl(dictFoldable)(function (acc) {
                return function (x) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableFirst = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldableLast = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Monoid_Endo.newtypeEndo)(Data_Newtype.unwrap(Data_Monoid_Dual.newtypeDual)(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($176) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(c)($176)));
                })(xs)))(u);
            };
        };
    };
};
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Monoid_Endo.newtypeEndo)(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo)(function ($177) {
                    return Data_Monoid_Endo.Endo(c($177));
                })(xs))(u);
            };
        };
    };
};
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Control_Category.id(Control_Category.categoryFn));
    };
};
var findMap = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return p(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};
var find = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing && p(v1)) {
                    return new Data_Maybe.Just(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};
var any = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Monoid_Disj.newtypeDisj)(Data_Monoid_Disj.newtypeDisj)(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
    };
};
var elem = function (dictFoldable) {
    return function (dictEq) {
        return function ($178) {
            return any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Eq.eq(dictEq)($178));
        };
    };
};
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            return function ($179) {
                return !elem(dictFoldable)(dictEq)(x)($179);
            };
        };
    };
};
var or = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return any(dictFoldable)(dictHeytingAlgebra)(Control_Category.id(Control_Category.categoryFn));
    };
};
var all = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Monoid_Conj.newtypeConj)(Data_Monoid_Conj.newtypeConj)(Data_Monoid_Conj.Conj)(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra)));
    };
};
var and = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return all(dictFoldable)(dictHeytingAlgebra)(Control_Category.id(Control_Category.categoryFn));
    };
};
module.exports = {
    Foldable: Foldable, 
    all: all, 
    and: and, 
    any: any, 
    elem: elem, 
    find: find, 
    findMap: findMap, 
    fold: fold, 
    foldMap: foldMap, 
    foldMapDefaultL: foldMapDefaultL, 
    foldMapDefaultR: foldMapDefaultR, 
    foldl: foldl, 
    foldlDefault: foldlDefault, 
    foldr: foldr, 
    foldrDefault: foldrDefault, 
    for_: for_, 
    intercalate: intercalate, 
    length: length, 
    maximum: maximum, 
    maximumBy: maximumBy, 
    minimum: minimum, 
    minimumBy: minimumBy, 
    notElem: notElem, 
    "null": $$null, 
    oneOf: oneOf, 
    or: or, 
    product: product, 
    sequence_: sequence_, 
    sum: sum, 
    traverse_: traverse_, 
    foldableArray: foldableArray, 
    foldableMaybe: foldableMaybe, 
    foldableFirst: foldableFirst, 
    foldableLast: foldableLast, 
    foldableAdditive: foldableAdditive, 
    foldableDual: foldableDual, 
    foldableDisj: foldableDisj, 
    foldableConj: foldableConj, 
    foldableMultiplicative: foldableMultiplicative
};

},{"../Control.Alt":4,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Control.Plus":41,"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Maybe.First":107,"../Data.Maybe.Last":108,"../Data.Monoid":116,"../Data.Monoid.Additive":110,"../Data.Monoid.Conj":111,"../Data.Monoid.Disj":112,"../Data.Monoid.Dual":113,"../Data.Monoid.Endo":114,"../Data.Monoid.Multiplicative":115,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Unit":149,"../Prelude":171,"./foreign":82}],84:[function(require,module,exports){
"use strict";

exports.toForeign = function (value) {
  return value;
};

exports.unsafeFromForeign = function (value) {
  return value;
};

exports.typeOf = function (value) {
  return typeof value;
};

exports.tagOf = function (value) {
  return Object.prototype.toString.call(value).slice(8, -1);
};

exports.isNull = function (value) {
  return value === null;
};

exports.isUndefined = function (value) {
  return value === undefined;
};

exports.isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

},{}],85:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Except = require("../Control.Monad.Except");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_List_NonEmpty = require("../Data.List.NonEmpty");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Prelude = require("../Prelude");
var ForeignError = (function () {
    function ForeignError(value0) {
        this.value0 = value0;
    };
    ForeignError.create = function (value0) {
        return new ForeignError(value0);
    };
    return ForeignError;
})();
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var JSONError = (function () {
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    return JSONError;
})();
var showForeignError = new Data_Show.Show(function (v) {
    if (v instanceof ForeignError) {
        return "(ForeignError " + (v.value0 + ")");
    };
    if (v instanceof ErrorAtIndex) {
        return "(ErrorAtIndex " + (Data_Show.show(Data_Show.showInt)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof ErrorAtProperty) {
        return "(ErrorAtProperty " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof JSONError) {
        return "(JSONError " + (Data_Show.show(Data_Show.showString)(v.value0) + ")");
    };
    if (v instanceof TypeMismatch) {
        return "(TypeMismatch " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at Data.Foreign line 65, column 3 - line 66, column 3: " + [ v.constructor.name ]);
});
var renderForeignError = function (v) {
    if (v instanceof ForeignError) {
        return v.value0;
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + Data_Show.show(showForeignError)(v.value1)));
    };
    if (v instanceof JSONError) {
        return "JSON error: " + v.value0;
    };
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    throw new Error("Failed pattern match at Data.Foreign line 75, column 1 - line 75, column 44: " + [ v.constructor.name ]);
};
var readUndefined = function (value) {
    if ($foreign.isUndefined(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Data.Foreign line 156, column 1 - line 158, column 34: " + [ value.constructor.name ]);
};
var readNullOrUndefined = function (value) {
    if ($foreign.isNull(value) || $foreign.isUndefined(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Data.Foreign line 161, column 1 - line 163, column 34: " + [ value.constructor.name ]);
};
var readNull = function (value) {
    if ($foreign.isNull(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Data.Foreign line 151, column 1 - line 153, column 34: " + [ value.constructor.name ]);
};
var fail = function ($121) {
    return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Data_Identity.monadIdentity))(Data_List_NonEmpty.singleton($121));
};
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
    };
    if (Data_Boolean.otherwise) {
        return fail(new TypeMismatch("array", $foreign.tagOf(value)));
    };
    throw new Error("Failed pattern match at Data.Foreign line 146, column 1 - line 148, column 58: " + [ value.constructor.name ]);
};
var unsafeReadTagged = function (tag) {
    return function (value) {
        if ($foreign.tagOf(value) === tag) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
        };
        if (Data_Boolean.otherwise) {
            return fail(new TypeMismatch(tag, $foreign.tagOf(value)));
        };
        throw new Error("Failed pattern match at Data.Foreign line 105, column 1 - line 107, column 54: " + [ tag.constructor.name, value.constructor.name ]);
    };
};
var readBoolean = unsafeReadTagged("Boolean");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Int", $foreign.tagOf(value))));
    var fromNumber = function ($122) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_Int.fromNumber($122));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromNumber))(readNumber(value));
};
var readString = unsafeReadTagged("String");
var readChar = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Char", $foreign.tagOf(value))));
    var fromString = function ($123) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_String.toChar($123));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromString))(readString(value));
};
var eqForeignError = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return x.value0 === y.value0;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            return x.value0 === y.value0 && x.value1 === y.value1;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return x.value0 === y.value0;
        };
        return false;
    };
});
var ordForeignError = new Data_Ord.Ord(function () {
    return eqForeignError;
}, function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        if (x instanceof ForeignError) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ForeignError) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
        };
        if (x instanceof TypeMismatch) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof TypeMismatch) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            var v = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtIndex) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtIndex) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtProperty) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof JSONError && y instanceof JSONError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        throw new Error("Failed pattern match: " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    ForeignError: ForeignError, 
    TypeMismatch: TypeMismatch, 
    ErrorAtIndex: ErrorAtIndex, 
    ErrorAtProperty: ErrorAtProperty, 
    JSONError: JSONError, 
    fail: fail, 
    readArray: readArray, 
    readBoolean: readBoolean, 
    readChar: readChar, 
    readInt: readInt, 
    readNull: readNull, 
    readNullOrUndefined: readNullOrUndefined, 
    readNumber: readNumber, 
    readString: readString, 
    readUndefined: readUndefined, 
    renderForeignError: renderForeignError, 
    unsafeReadTagged: unsafeReadTagged, 
    eqForeignError: eqForeignError, 
    ordForeignError: ordForeignError, 
    showForeignError: showForeignError, 
    isArray: $foreign.isArray, 
    isNull: $foreign.isNull, 
    isUndefined: $foreign.isUndefined, 
    tagOf: $foreign.tagOf, 
    toForeign: $foreign.toForeign, 
    typeOf: $foreign.typeOf, 
    unsafeFromForeign: $foreign.unsafeFromForeign
};

},{"../Control.Applicative":6,"../Control.Monad.Error.Class":28,"../Control.Monad.Except":30,"../Control.Monad.Except.Trans":29,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Either":75,"../Data.Eq":78,"../Data.Function":88,"../Data.HeytingAlgebra":96,"../Data.Identity":97,"../Data.Int":101,"../Data.List.NonEmpty":102,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Data.String":139,"../Prelude":171,"./foreign":84}],86:[function(require,module,exports){
"use strict";

// module Data.Function.Uncurried

exports.mkFn0 = function (fn) {
  return function () {
    return fn({});
  };
};

exports.mkFn2 = function (fn) {
  /* jshint maxparams: 2 */
  return function (a, b) {
    return fn(a)(b);
  };
};

exports.mkFn3 = function (fn) {
  /* jshint maxparams: 3 */
  return function (a, b, c) {
    return fn(a)(b)(c);
  };
};

exports.mkFn4 = function (fn) {
  /* jshint maxparams: 4 */
  return function (a, b, c, d) {
    return fn(a)(b)(c)(d);
  };
};

exports.mkFn5 = function (fn) {
  /* jshint maxparams: 5 */
  return function (a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

exports.mkFn6 = function (fn) {
  /* jshint maxparams: 6 */
  return function (a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f);
  };
};

exports.mkFn7 = function (fn) {
  /* jshint maxparams: 7 */
  return function (a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g);
  };
};

exports.mkFn8 = function (fn) {
  /* jshint maxparams: 8 */
  return function (a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h);
  };
};

exports.mkFn9 = function (fn) {
  /* jshint maxparams: 9 */
  return function (a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
  };
};

exports.mkFn10 = function (fn) {
  /* jshint maxparams: 10 */
  return function (a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
  };
};

exports.runFn0 = function (fn) {
  return fn();
};

exports.runFn2 = function (fn) {
  return function (a) {
    return function (b) {
      return fn(a, b);
    };
  };
};

exports.runFn3 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return fn(a, b, c);
      };
    };
  };
};

exports.runFn4 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

exports.runFn5 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

exports.runFn6 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return fn(a, b, c, d, e, f);
            };
          };
        };
      };
    };
  };
};

exports.runFn7 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return fn(a, b, c, d, e, f, g);
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn8 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return fn(a, b, c, d, e, f, g, h);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn9 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return fn(a, b, c, d, e, f, g, h, i);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn10 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return fn(a, b, c, d, e, f, g, h, i, j);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

},{}],87:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var runFn1 = function (f) {
    return f;
};
var mkFn1 = function (f) {
    return f;
};
module.exports = {
    mkFn1: mkFn1, 
    runFn1: runFn1, 
    mkFn0: $foreign.mkFn0, 
    mkFn10: $foreign.mkFn10, 
    mkFn2: $foreign.mkFn2, 
    mkFn3: $foreign.mkFn3, 
    mkFn4: $foreign.mkFn4, 
    mkFn5: $foreign.mkFn5, 
    mkFn6: $foreign.mkFn6, 
    mkFn7: $foreign.mkFn7, 
    mkFn8: $foreign.mkFn8, 
    mkFn9: $foreign.mkFn9, 
    runFn0: $foreign.runFn0, 
    runFn10: $foreign.runFn10, 
    runFn2: $foreign.runFn2, 
    runFn3: $foreign.runFn3, 
    runFn4: $foreign.runFn4, 
    runFn5: $foreign.runFn5, 
    runFn6: $foreign.runFn6, 
    runFn7: $foreign.runFn7, 
    runFn8: $foreign.runFn8, 
    runFn9: $foreign.runFn9
};

},{"../Data.Unit":149,"./foreign":86}],88:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var $$const = function (a) {
    return function (v) {
        return a;
    };
};
var applyFlipped = function (x) {
    return function (f) {
        return f(x);
    };
};
var apply = function (f) {
    return function (x) {
        return f(x);
    };
};
module.exports = {
    apply: apply, 
    applyFlipped: applyFlipped, 
    "const": $$const, 
    flip: flip, 
    on: on
};

},{"../Control.Category":13}],89:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Functor = require("../Data.Functor");
var Invariant = function (imap) {
    this.imap = imap;
};
var imapF = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(f);
        };
    };
};
var invariantArray = new Invariant(imapF(Data_Functor.functorArray));
var invariantFn = new Invariant(imapF(Data_Functor.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    Invariant: Invariant, 
    imap: imap, 
    imapF: imapF, 
    invariantFn: invariantFn, 
    invariantArray: invariantArray
};

},{"../Data.Functor":91}],90:[function(require,module,exports){
"use strict";

exports.arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

},{}],91:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Unit = require("../Data.Unit");
var Functor = function (map) {
    this.map = map;
};
var map = function (dict) {
    return dict.map;
};
var mapFlipped = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return map(dictFunctor)(f)(fa);
        };
    };
};
var $$void = function (dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};
var voidLeft = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return map(dictFunctor)(Data_Function["const"](x))(f);
        };
    };
};
var voidRight = function (dictFunctor) {
    return function (x) {
        return map(dictFunctor)(Data_Function["const"](x));
    };
};
var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
var functorArray = new Functor($foreign.arrayMap);
var flap = function (dictFunctor) {
    return function (ff) {
        return function (x) {
            return map(dictFunctor)(function (f) {
                return f(x);
            })(ff);
        };
    };
};
module.exports = {
    Functor: Functor, 
    flap: flap, 
    map: map, 
    mapFlipped: mapFlipped, 
    "void": $$void, 
    voidLeft: voidLeft, 
    voidRight: voidRight, 
    functorFn: functorFn, 
    functorArray: functorArray
};

},{"../Control.Semigroupoid":42,"../Data.Function":88,"../Data.Unit":149,"./foreign":90}],92:[function(require,module,exports){
"use strict";

// module Data.Generic

exports.zipAll = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      for (var i = 0; i < l; i++) {
        if (!f(xs[i])(ys[i])) {
          return false;
        }
      }
      return true;
    };
  };
};

exports.zipCompare = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var o = f(xs[i])(ys[i]);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

},{}],93:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Identity = require("../Data.Identity");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Prelude = require("../Prelude");
var Type_Proxy = require("../Type.Proxy");
var SProd = (function () {
    function SProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SProd.create = function (value0) {
        return function (value1) {
            return new SProd(value0, value1);
        };
    };
    return SProd;
})();
var SRecord = (function () {
    function SRecord(value0) {
        this.value0 = value0;
    };
    SRecord.create = function (value0) {
        return new SRecord(value0);
    };
    return SRecord;
})();
var SNumber = (function () {
    function SNumber(value0) {
        this.value0 = value0;
    };
    SNumber.create = function (value0) {
        return new SNumber(value0);
    };
    return SNumber;
})();
var SBoolean = (function () {
    function SBoolean(value0) {
        this.value0 = value0;
    };
    SBoolean.create = function (value0) {
        return new SBoolean(value0);
    };
    return SBoolean;
})();
var SInt = (function () {
    function SInt(value0) {
        this.value0 = value0;
    };
    SInt.create = function (value0) {
        return new SInt(value0);
    };
    return SInt;
})();
var SString = (function () {
    function SString(value0) {
        this.value0 = value0;
    };
    SString.create = function (value0) {
        return new SString(value0);
    };
    return SString;
})();
var SChar = (function () {
    function SChar(value0) {
        this.value0 = value0;
    };
    SChar.create = function (value0) {
        return new SChar(value0);
    };
    return SChar;
})();
var SArray = (function () {
    function SArray(value0) {
        this.value0 = value0;
    };
    SArray.create = function (value0) {
        return new SArray(value0);
    };
    return SArray;
})();
var SUnit = (function () {
    function SUnit() {

    };
    SUnit.value = new SUnit();
    return SUnit;
})();
var SigProd = (function () {
    function SigProd(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SigProd.create = function (value0) {
        return function (value1) {
            return new SigProd(value0, value1);
        };
    };
    return SigProd;
})();
var SigRecord = (function () {
    function SigRecord(value0) {
        this.value0 = value0;
    };
    SigRecord.create = function (value0) {
        return new SigRecord(value0);
    };
    return SigRecord;
})();
var SigNumber = (function () {
    function SigNumber() {

    };
    SigNumber.value = new SigNumber();
    return SigNumber;
})();
var SigBoolean = (function () {
    function SigBoolean() {

    };
    SigBoolean.value = new SigBoolean();
    return SigBoolean;
})();
var SigInt = (function () {
    function SigInt() {

    };
    SigInt.value = new SigInt();
    return SigInt;
})();
var SigString = (function () {
    function SigString() {

    };
    SigString.value = new SigString();
    return SigString;
})();
var SigChar = (function () {
    function SigChar() {

    };
    SigChar.value = new SigChar();
    return SigChar;
})();
var SigArray = (function () {
    function SigArray(value0) {
        this.value0 = value0;
    };
    SigArray.create = function (value0) {
        return new SigArray(value0);
    };
    return SigArray;
})();
var SigUnit = (function () {
    function SigUnit() {

    };
    SigUnit.value = new SigUnit();
    return SigUnit;
})();
var Generic = function (fromSpine, toSignature, toSpine) {
    this.fromSpine = fromSpine;
    this.toSignature = toSignature;
    this.toSpine = toSpine;
};
var toSpine = function (dict) {
    return dict.toSpine;
};
var toSignature = function (dict) {
    return dict.toSignature;
};
var showSuspended = function (dictShow) {
    return function (e) {
        return "\\_ -> " + Data_Show.show(dictShow)(e(Data_Unit.unit));
    };
};
var showArray = function (v) {
    return function (v1) {
        if (v1.length === 0) {
            return "[]";
        };
        return "[ " + (Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(", ")(Data_Functor.map(Data_Functor.functorArray)(v)(v1)) + " ]");
    };
};
var showGenericSpine = new Data_Show.Show(function (v) {
    if (v instanceof SUnit) {
        return "SUnit";
    };
    if (v instanceof SChar) {
        return "SChar " + Data_Show.show(Data_Show.showChar)(v.value0);
    };
    if (v instanceof SString) {
        return "SString " + Data_Show.show(Data_Show.showString)(v.value0);
    };
    if (v instanceof SBoolean) {
        return "SBoolean " + Data_Show.show(Data_Show.showBoolean)(v.value0);
    };
    if (v instanceof SNumber) {
        return "SNumber " + Data_Show.show(Data_Show.showNumber)(v.value0);
    };
    if (v instanceof SInt) {
        return "SInt " + Data_Show.show(Data_Show.showInt)(v.value0);
    };
    if (v instanceof SArray) {
        return "SArray " + showArray(showSuspended(showGenericSpine))(v.value0);
    };
    if (v instanceof SProd) {
        return "SProd " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + showArray(showSuspended(showGenericSpine))(v.value1)));
    };
    if (v instanceof SRecord) {
        var showElt = function (v1) {
            return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)([ "{ recLabel: ", Data_Show.show(Data_Show.showString)(v1.recLabel), ", recValue: ", showSuspended(showGenericSpine)(v1.recValue), " }" ]);
        };
        return "SRecord " + showArray(showElt)(v.value0);
    };
    throw new Error("Failed pattern match at Data.Generic line 272, column 9 - line 273, column 9: " + [ v.constructor.name ]);
});
var orderingToInt = function (v) {
    if (v instanceof Data_Ordering.EQ) {
        return 0;
    };
    if (v instanceof Data_Ordering.LT) {
        return 1;
    };
    if (v instanceof Data_Ordering.GT) {
        return -1 | 0;
    };
    throw new Error("Failed pattern match at Data.Generic line 538, column 17 - line 541, column 10: " + [ v.constructor.name ]);
};
var genericVoid = new Generic(function (v) {
    return Data_Maybe.Nothing.value;
}, function (v) {
    return new SigProd("Data.Void.Void", [  ]);
}, Data_Void.absurd);
var genericUnit = new Generic(function (v) {
    if (v instanceof SUnit) {
        return new Data_Maybe.Just(Data_Unit.unit);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigUnit.value;
}, function (v) {
    return SUnit.value;
});
var genericString = new Generic(function (v) {
    if (v instanceof SString) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigString.value;
}, SString.create);
var genericOrdering = new Generic(function (v) {
    if (v instanceof SProd && (v.value0 === "Data.Ordering.LT" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.LT.value);
    };
    if (v instanceof SProd && (v.value0 === "Data.Ordering.EQ" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.EQ.value);
    };
    if (v instanceof SProd && (v.value0 === "Data.Ordering.GT" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Data_Ordering.GT.value);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return new SigProd("Data.Ordering.Ordering", [ {
        sigConstructor: "Data.Ordering.LT", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Ordering.EQ", 
        sigValues: [  ]
    }, {
        sigConstructor: "Data.Ordering.GT", 
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Data_Ordering.LT) {
        return new SProd("Data.Ordering.LT", [  ]);
    };
    if (v instanceof Data_Ordering.EQ) {
        return new SProd("Data.Ordering.EQ", [  ]);
    };
    if (v instanceof Data_Ordering.GT) {
        return new SProd("Data.Ordering.GT", [  ]);
    };
    throw new Error("Failed pattern match at Data.Generic line 218, column 13 - line 221, column 38: " + [ v.constructor.name ]);
});
var genericNumber = new Generic(function (v) {
    if (v instanceof SNumber) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigNumber.value;
}, SNumber.create);
var genericInt = new Generic(function (v) {
    if (v instanceof SInt) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigInt.value;
}, SInt.create);
var genericChar = new Generic(function (v) {
    if (v instanceof SChar) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigChar.value;
}, SChar.create);
var genericBool = new Generic(function (v) {
    if (v instanceof SBoolean) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
}, function (v) {
    return SigBoolean.value;
}, SBoolean.create);
var fromSpine = function (dict) {
    return dict.fromSpine;
};
var force = function (f) {
    return f(Data_Unit.unit);
};
var genericArray = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SArray) {
            return Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function ($310) {
                return fromSpine(dictGeneric)(force($310));
            })(v.value0);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var lowerProxy = function (v) {
            return Type_Proxy["Proxy"].value;
        };
        return new SigArray(function (v) {
            return toSignature(dictGeneric)(lowerProxy(x));
        });
    }, function ($311) {
        return SArray.create(Data_Functor.map(Data_Functor.functorArray)(function (x) {
            return function (v) {
                return toSpine(dictGeneric)(x);
            };
        })($311));
    });
};
var genericEither = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Either.Left" && v.value1.length === 1)) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Left.create)(fromSpine(dictGeneric)(force(v["value1"][0])));
            };
            if (v instanceof SProd && (v.value0 === "Data.Either.Right" && v.value1.length === 1)) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Either.Right.create)(fromSpine(dictGeneric1)(force(v["value1"][0])));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var rproxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            var lproxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            return new SigProd("Data.Either.Either", [ {
                sigConstructor: "Data.Either.Left", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric)(lproxy(x));
                } ]
            }, {
                sigConstructor: "Data.Either.Right", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric1)(rproxy(x));
                } ]
            } ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return new SProd("Data.Either.Left", [ function (v1) {
                    return toSpine(dictGeneric)(v.value0);
                } ]);
            };
            if (v instanceof Data_Either.Right) {
                return new SProd("Data.Either.Right", [ function (v1) {
                    return toSpine(dictGeneric1)(v.value0);
                } ]);
            };
            throw new Error("Failed pattern match at Data.Generic line 181, column 3 - line 181, column 64: " + [ v.constructor.name ]);
        });
    };
};
var genericIdentity = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.Identity.Identity" && v.value1.length === 1)) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Identity.Identity)(fromSpine(dictGeneric)(force(v["value1"][0])));
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var iproxy = function (v) {
            return Type_Proxy["Proxy"].value;
        };
        return new SigProd("Data.Identity.Identity", [ {
            sigConstructor: "Data.Identity.Identity", 
            sigValues: [ function (v) {
                return toSignature(dictGeneric)(iproxy(x));
            } ]
        } ]);
    }, function (v) {
        return new SProd("Data.Identity.Identity", [ function (v1) {
            return toSpine(dictGeneric)(v);
        } ]);
    });
};
var genericList = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.List.Types.Cons" && v.value1.length === 2)) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_List_Types.Cons.create)(fromSpine(dictGeneric)(force(v["value1"][0]))))(fromSpine(genericList(dictGeneric))(force(v["value1"][1])));
        };
        if (v instanceof SProd && (v.value0 === "Data.List.Types.Nil" && v.value1.length === 0)) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_List_Types.Nil.value);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var headProxy = function (v) {
            return Type_Proxy["Proxy"].value;
        };
        return new SigProd("Data.List.Types.List", [ {
            sigConstructor: "Data.List.Types.Cons", 
            sigValues: [ function (v) {
                return toSignature(dictGeneric)(headProxy(x));
            }, function (v) {
                return toSignature(genericList(dictGeneric))(x);
            } ]
        }, {
            sigConstructor: "Data.List.Types.Nil", 
            sigValues: [  ]
        } ]);
    }, function (v) {
        if (v instanceof Data_List_Types.Cons) {
            return new SProd("Data.List.Types.Cons", [ function (v1) {
                return toSpine(dictGeneric)(v.value0);
            }, function (v1) {
                return toSpine(genericList(dictGeneric))(v.value1);
            } ]);
        };
        if (v instanceof Data_List_Types.Nil) {
            return new SProd("Data.List.Types.Nil", [  ]);
        };
        throw new Error("Failed pattern match at Data.Generic line 117, column 3 - line 118, column 68: " + [ v.constructor.name ]);
    });
};
var genericMaybe = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Just" && v.value1.length === 1)) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(fromSpine(dictGeneric)(force(v["value1"][0])));
        };
        if (v instanceof SProd && (v.value0 === "Data.Maybe.Nothing" && v.value1.length === 0)) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var mbProxy = function (v) {
            return Type_Proxy["Proxy"].value;
        };
        return new SigProd("Data.Maybe.Maybe", [ {
            sigConstructor: "Data.Maybe.Just", 
            sigValues: [ function (v) {
                return toSignature(dictGeneric)(mbProxy(x));
            } ]
        }, {
            sigConstructor: "Data.Maybe.Nothing", 
            sigValues: [  ]
        } ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Just) {
            return new SProd("Data.Maybe.Just", [ function (v1) {
                return toSpine(dictGeneric)(v.value0);
            } ]);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return new SProd("Data.Maybe.Nothing", [  ]);
        };
        throw new Error("Failed pattern match at Data.Generic line 161, column 3 - line 161, column 63: " + [ v.constructor.name ]);
    });
};
var genericNonEmpty = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.NonEmpty.NonEmpty" && v.value1.length === 2)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_NonEmpty.NonEmpty.create)(fromSpine(dictGeneric1)(force(v["value1"][0]))))(fromSpine(dictGeneric)(force(v["value1"][1])));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var tailProxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            var headProxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            return new SigProd("Data.NonEmpty.NonEmpty", [ {
                sigConstructor: "Data.NonEmpty.NonEmpty", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric1)(headProxy(x));
                }, function (v) {
                    return toSignature(dictGeneric)(tailProxy(x));
                } ]
            } ]);
        }, function (v) {
            return new SProd("Data.NonEmpty.NonEmpty", [ function (v1) {
                return toSpine(dictGeneric1)(v.value0);
            }, function (v1) {
                return toSpine(dictGeneric)(v.value1);
            } ]);
        });
    };
};
var genericNonEmptyList = function (dictGeneric) {
    return new Generic(function (v) {
        if (v instanceof SProd && (v.value0 === "Data.List.Types.NonEmptyList" && v.value1.length === 1)) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_List_Types.NonEmptyList)(fromSpine(genericNonEmpty(genericList(dictGeneric))(dictGeneric))(force(v["value1"][0])));
        };
        return Data_Maybe.Nothing.value;
    }, function (x) {
        var listProxy = function (v) {
            return Type_Proxy["Proxy"].value;
        };
        return new SigProd("Data.List.Types.NonEmptyList", [ {
            sigConstructor: "Data.List.Types.NonEmptyList", 
            sigValues: [ function (v) {
                return toSignature(genericList(dictGeneric))(listProxy(x));
            } ]
        } ]);
    }, function (v) {
        return new SProd("Data.List.Types.NonEmptyList", [ function (v1) {
            return toSpine(genericNonEmpty(genericList(dictGeneric))(dictGeneric))(v);
        } ]);
    });
};
var genericShowPrec = function (v) {
    return function (v1) {
        if (v1 instanceof SProd) {
            if (Data_Array["null"](v1.value1)) {
                return v1.value0;
            };
            if (Data_Boolean.otherwise) {
                var showParen = function (v2) {
                    return function (x) {
                        if (!v2) {
                            return x;
                        };
                        if (v2) {
                            return "(" + (x + ")");
                        };
                        throw new Error("Failed pattern match at Data.Generic line 467, column 7 - line 467, column 28: " + [ v2.constructor.name, x.constructor.name ]);
                    };
                };
                return showParen(v > 10)(v1.value0 + (" " + Data_String.joinWith(" ")(Data_Functor.map(Data_Functor.functorArray)(function (x) {
                    return genericShowPrec(11)(force(x));
                })(v1.value1))));
            };
        };
        if (v1 instanceof SRecord) {
            var showLabelPart = function (x) {
                return x.recLabel + (": " + genericShowPrec(0)(force(x.recValue)));
            };
            return "{" + (Data_String.joinWith(", ")(Data_Functor.map(Data_Functor.functorArray)(showLabelPart)(v1.value0)) + "}");
        };
        if (v1 instanceof SBoolean) {
            return Data_Show.show(Data_Show.showBoolean)(v1.value0);
        };
        if (v1 instanceof SInt) {
            return Data_Show.show(Data_Show.showInt)(v1.value0);
        };
        if (v1 instanceof SNumber) {
            return Data_Show.show(Data_Show.showNumber)(v1.value0);
        };
        if (v1 instanceof SString) {
            return Data_Show.show(Data_Show.showString)(v1.value0);
        };
        if (v1 instanceof SChar) {
            return Data_Show.show(Data_Show.showChar)(v1.value0);
        };
        if (v1 instanceof SArray) {
            return "[" + (Data_String.joinWith(", ")(Data_Functor.map(Data_Functor.functorArray)(function (x) {
                return genericShowPrec(0)(force(x));
            })(v1.value0)) + "]");
        };
        if (v1 instanceof SUnit) {
            return "unit";
        };
        throw new Error("Failed pattern match at Data.Generic line 461, column 1 - line 469, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var gShow = function (dictGeneric) {
    return function ($312) {
        return genericShowPrec(0)(toSpine(dictGeneric)($312));
    };
};
var genericTuple = function (dictGeneric) {
    return function (dictGeneric1) {
        return new Generic(function (v) {
            if (v instanceof SProd && (v.value0 === "Data.Tuple.Tuple" && v.value1.length === 2)) {
                return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(fromSpine(dictGeneric)(force(v["value1"][0]))))(fromSpine(dictGeneric1)(force(v["value1"][1])));
            };
            return Data_Maybe.Nothing.value;
        }, function (x) {
            var sndProxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            var fstProxy = function (v) {
                return Type_Proxy["Proxy"].value;
            };
            return new SigProd("Data.Tuple.Tuple", [ {
                sigConstructor: "Data.Tuple.Tuple", 
                sigValues: [ function (v) {
                    return toSignature(dictGeneric)(fstProxy(x));
                }, function (v) {
                    return toSignature(dictGeneric1)(sndProxy(x));
                } ]
            } ]);
        }, function (v) {
            return new SProd("Data.Tuple.Tuple", [ function (v1) {
                return toSpine(dictGeneric)(v.value0);
            }, function (v1) {
                return toSpine(dictGeneric1)(v.value1);
            } ]);
        });
    };
};
var isValidSpine = function (v) {
    return function (v1) {
        if (v instanceof SigBoolean && v1 instanceof SBoolean) {
            return true;
        };
        if (v instanceof SigNumber && v1 instanceof SNumber) {
            return true;
        };
        if (v instanceof SigInt && v1 instanceof SInt) {
            return true;
        };
        if (v instanceof SigString && v1 instanceof SString) {
            return true;
        };
        if (v instanceof SigChar && v1 instanceof SChar) {
            return true;
        };
        if (v instanceof SigArray && v1 instanceof SArray) {
            return Data_Foldable.all(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(function ($313) {
                return isValidSpine(force(v.value0))(force($313));
            })(v1.value0);
        };
        if (v instanceof SigProd && v1 instanceof SProd) {
            var v2 = Data_Foldable.find(Data_Foldable.foldableArray)(function (alt) {
                return alt.sigConstructor === v1.value0;
            })(v.value1);
            if (v2 instanceof Data_Maybe.Nothing) {
                return false;
            };
            if (v2 instanceof Data_Maybe.Just) {
                return Data_Foldable.and(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Array.zipWith(function (sig) {
                    return function (spine) {
                        return isValidSpine(force(sig))(force(spine));
                    };
                })(v2.value0.sigValues)(v1.value1));
            };
            throw new Error("Failed pattern match at Data.Generic line 438, column 3 - line 444, column 15: " + [ v2.constructor.name ]);
        };
        if (v instanceof SigRecord && v1 instanceof SRecord) {
            return Data_Foldable.and(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Array.zipWith(function (sig) {
                return function (val) {
                    return isValidSpine(force(sig.recValue))(force(val.recValue));
                };
            })(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Data_Ord.compare(Data_Ord.ordString)(a.recLabel)(b.recLabel);
                };
            })(v.value0))(Data_Array.sortBy(function (a) {
                return function (b) {
                    return Data_Ord.compare(Data_Ord.ordString)(a.recLabel)(b.recLabel);
                };
            })(v1.value0)));
        };
        if (v instanceof SigUnit && v1 instanceof SUnit) {
            return true;
        };
        return false;
    };
};
var showSignature = function (sig) {
    var needsParen = function (s) {
        if (s instanceof SigProd) {
            return true;
        };
        if (s instanceof SigRecord) {
            return true;
        };
        if (s instanceof SigNumber) {
            return false;
        };
        if (s instanceof SigBoolean) {
            return false;
        };
        if (s instanceof SigInt) {
            return false;
        };
        if (s instanceof SigString) {
            return false;
        };
        if (s instanceof SigChar) {
            return false;
        };
        if (s instanceof SigArray) {
            return true;
        };
        if (s instanceof SigUnit) {
            return false;
        };
        throw new Error("Failed pattern match at Data.Generic line 403, column 18 - line 412, column 21: " + [ s.constructor.name ]);
    };
    var paren = function (s) {
        if (needsParen(s)) {
            return "(" + (showSignature(s) + ")");
        };
        if (Data_Boolean.otherwise) {
            return showSignature(s);
        };
        throw new Error("Failed pattern match at Data.Generic line 386, column 3 - line 412, column 21: " + [ s.constructor.name ]);
    };
    return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)((function () {
        if (sig instanceof SigProd) {
            return [ "SigProd ", Data_Show.show(Data_Show.showString)(sig.value0), " ", showArray(showDataConstructor)(sig.value1) ];
        };
        if (sig instanceof SigRecord) {
            return [ "SigRecord ", showArray(showLabel)(sig.value0) ];
        };
        if (sig instanceof SigNumber) {
            return [ "SigNumber" ];
        };
        if (sig instanceof SigBoolean) {
            return [ "SigBoolean" ];
        };
        if (sig instanceof SigInt) {
            return [ "SigInt" ];
        };
        if (sig instanceof SigString) {
            return [ "SigString" ];
        };
        if (sig instanceof SigChar) {
            return [ "SigChar" ];
        };
        if (sig instanceof SigArray) {
            return [ "SigArray ", paren(force(sig.value0)) ];
        };
        if (sig instanceof SigUnit) {
            return [ "SigUnit" ];
        };
        throw new Error("Failed pattern match at Data.Generic line 386, column 10 - line 396, column 27: " + [ sig.constructor.name ]);
    })());
};
var showLabel = function (l) {
    return "{ recLabel: " + (Data_Show.show(Data_Show.showString)(l.recLabel) + (", recValue: " + (showSignature(force(l.recValue)) + " }")));
};
var showDataConstructor = function (dc) {
    return "{ sigConstructor: " + (Data_Show.show(Data_Show.showString)(dc.sigConstructor) + (", sigValues: " + (showArray(function ($314) {
        return showSignature(force($314));
    })(dc.sigValues) + " }")));
};
var showGenericSignature = new Data_Show.Show(showSignature);
var eqThunk = function (dictEq) {
    return function (x) {
        return function (y) {
            return Data_Eq.eq(dictEq)(force(x))(force(y));
        };
    };
};
var eqRecordSigs = function (dictEq) {
    return function (arr1) {
        return function (arr2) {
            var labelCompare = function (r1) {
                return function (r2) {
                    return Data_Ord.compare(Data_Ord.ordString)(r1.recLabel)(r2.recLabel);
                };
            };
            var sorted1 = Data_Array.sortBy(labelCompare)(arr1);
            var sorted2 = Data_Array.sortBy(labelCompare)(arr2);
            var doCmp = function (x) {
                return function (y) {
                    return x.recLabel === y.recLabel && Data_Eq.eq(dictEq)(force(x.recValue))(force(y.recValue));
                };
            };
            return Data_Array.length(arr1) === Data_Array.length(arr2) && $foreign.zipAll(doCmp)(sorted1)(sorted2);
        };
    };
};
var eqGenericSpine = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            return v.value0 === v1.value0 && (Data_Array.length(v.value1) === Data_Array.length(v1.value1) && $foreign.zipAll(eqThunk(eqGenericSpine))(v.value1)(v1.value1));
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            return eqRecordSigs(eqGenericSpine)(v.value0)(v1.value0);
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return v.value0 === v1.value0;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Data_Array.length(v.value0) === Data_Array.length(v1.value0) && $foreign.zipAll(eqThunk(eqGenericSpine))(v.value0)(v1.value0);
        };
        if (v instanceof SUnit && v1 instanceof SUnit) {
            return true;
        };
        return false;
    };
});
var gEq = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Data_Eq.eq(eqGenericSpine)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
var eqGenericSignature = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof SigProd && v1 instanceof SigProd) {
            return v.value0 === v1.value0 && (Data_Array.length(v.value1) === Data_Array.length(v1.value1) && $foreign.zipAll(eqDataConstructor)(v.value1)(v1.value1));
        };
        if (v instanceof SigRecord && v1 instanceof SigRecord) {
            return eqRecordSigs(eqGenericSignature)(v.value0)(v1.value0);
        };
        if (v instanceof SigNumber && v1 instanceof SigNumber) {
            return true;
        };
        if (v instanceof SigBoolean && v1 instanceof SigBoolean) {
            return true;
        };
        if (v instanceof SigInt && v1 instanceof SigInt) {
            return true;
        };
        if (v instanceof SigString && v1 instanceof SigString) {
            return true;
        };
        if (v instanceof SigChar && v1 instanceof SigChar) {
            return true;
        };
        if (v instanceof SigArray && v1 instanceof SigArray) {
            return eqThunk(eqGenericSignature)(v.value0)(v1.value0);
        };
        if (v instanceof SigUnit && v1 instanceof SigUnit) {
            return true;
        };
        return false;
    };
});
var eqDataConstructor = function (p1) {
    return function (p2) {
        return p1.sigConstructor === p2.sigConstructor && $foreign.zipAll(eqThunk(eqGenericSignature))(p1.sigValues)(p2.sigValues);
    };
};
var compareThunk = function (dictOrd) {
    return function (x) {
        return function (y) {
            return orderingToInt(Data_Ord.compare(dictOrd)(force(x))(force(y)));
        };
    };
};
var ordGenericSpine = new Data_Ord.Ord(function () {
    return eqGenericSpine;
}, function (v) {
    return function (v1) {
        if (v instanceof SProd && v1 instanceof SProd) {
            var v2 = Data_Ord.compare(Data_Ord.ordString)(v.value0)(v1.value0);
            if (v2 instanceof Data_Ordering.EQ) {
                return Data_Ord.compare(Data_Ord.ordInt)(0)($foreign.zipCompare(compareThunk(ordGenericSpine))(v.value1)(v1.value1));
            };
            return v2;
        };
        if (v instanceof SProd) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SProd) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SRecord && v1 instanceof SRecord) {
            var go = function (x) {
                return function (y) {
                    var v2 = Data_Ord.compare(Data_Ord.ordString)(x.recLabel)(y.recLabel);
                    if (v2 instanceof Data_Ordering.EQ) {
                        return orderingToInt(Data_Ord.compare(ordGenericSpine)(force(x.recValue))(force(y.recValue)));
                    };
                    return orderingToInt(v2);
                };
            };
            return Data_Ord.compare(Data_Ord.ordInt)(0)($foreign.zipCompare(go)(v.value0)(v1.value0));
        };
        if (v instanceof SRecord) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SRecord) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SInt && v1 instanceof SInt) {
            return Data_Ord.compare(Data_Ord.ordInt)(v.value0)(v1.value0);
        };
        if (v instanceof SInt) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SInt) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SBoolean && v1 instanceof SBoolean) {
            return Data_Ord.compare(Data_Ord.ordBoolean)(v.value0)(v1.value0);
        };
        if (v instanceof SBoolean) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SBoolean) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SNumber && v1 instanceof SNumber) {
            return Data_Ord.compare(Data_Ord.ordNumber)(v.value0)(v1.value0);
        };
        if (v instanceof SNumber) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SNumber) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SString && v1 instanceof SString) {
            return Data_Ord.compare(Data_Ord.ordString)(v.value0)(v1.value0);
        };
        if (v instanceof SString) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SString) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SChar && v1 instanceof SChar) {
            return Data_Ord.compare(Data_Ord.ordChar)(v.value0)(v1.value0);
        };
        if (v instanceof SChar) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SChar) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SArray && v1 instanceof SArray) {
            return Data_Ord.compare(Data_Ord.ordInt)(0)($foreign.zipCompare(compareThunk(ordGenericSpine))(v.value0)(v1.value0));
        };
        if (v instanceof SArray) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof SArray) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof SUnit && v1 instanceof SUnit) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Generic line 304, column 3 - line 307, column 15: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var gCompare = function (dictGeneric) {
    return function (x) {
        return function (y) {
            return Data_Ord.compare(ordGenericSpine)(toSpine(dictGeneric)(x))(toSpine(dictGeneric)(y));
        };
    };
};
module.exports = {
    SigProd: SigProd, 
    SigRecord: SigRecord, 
    SigNumber: SigNumber, 
    SigBoolean: SigBoolean, 
    SigInt: SigInt, 
    SigString: SigString, 
    SigChar: SigChar, 
    SigArray: SigArray, 
    SigUnit: SigUnit, 
    SProd: SProd, 
    SRecord: SRecord, 
    SNumber: SNumber, 
    SBoolean: SBoolean, 
    SInt: SInt, 
    SString: SString, 
    SChar: SChar, 
    SArray: SArray, 
    SUnit: SUnit, 
    Generic: Generic, 
    fromSpine: fromSpine, 
    gCompare: gCompare, 
    gEq: gEq, 
    gShow: gShow, 
    isValidSpine: isValidSpine, 
    showDataConstructor: showDataConstructor, 
    showSignature: showSignature, 
    toSignature: toSignature, 
    toSpine: toSpine, 
    genericNumber: genericNumber, 
    genericInt: genericInt, 
    genericString: genericString, 
    genericChar: genericChar, 
    genericBool: genericBool, 
    genericArray: genericArray, 
    genericUnit: genericUnit, 
    genericVoid: genericVoid, 
    genericTuple: genericTuple, 
    genericList: genericList, 
    genericNonEmptyList: genericNonEmptyList, 
    genericMaybe: genericMaybe, 
    genericEither: genericEither, 
    genericIdentity: genericIdentity, 
    genericOrdering: genericOrdering, 
    genericNonEmpty: genericNonEmpty, 
    showGenericSpine: showGenericSpine, 
    eqGenericSpine: eqGenericSpine, 
    ordGenericSpine: ordGenericSpine, 
    eqGenericSignature: eqGenericSignature, 
    showGenericSignature: showGenericSignature
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Semigroupoid":42,"../Data.Array":53,"../Data.Boolean":63,"../Data.Either":75,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Identity":97,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.NonEmpty":119,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Show":132,"../Data.String":139,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unit":149,"../Data.Void":150,"../Prelude":171,"../Type.Proxy":180,"./foreign":92}],94:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Boolean = require("../Data.Boolean");
var Data_CatList = require("../Data.CatList");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var Emit = (function () {
    function Emit(value0) {
        this.value0 = value0;
    };
    Emit.create = function (value0) {
        return new Emit(value0);
    };
    return Emit;
})();
var Visit = (function () {
    function Visit(value0) {
        this.value0 = value0;
    };
    Visit.create = function (value0) {
        return new Visit(value0);
    };
    return Visit;
})();
var Graph = function (x) {
    return x;
};
var vertices = function (v) {
    return Data_Functor.map(Data_List_Types.functorList)(Data_Tuple.fst)(Data_Map.values(v));
};
var unfoldGraph = function (dictOrd) {
    return function (dictFunctor) {
        return function (dictFoldable) {
            return function (dictFoldable1) {
                return function (ks) {
                    return function (label) {
                        return function (edges) {
                            return Data_Map.fromFoldable(dictOrd)(dictFoldable)(Data_Functor.map(dictFunctor)(function (k) {
                                return new Data_Tuple.Tuple(k, new Data_Tuple.Tuple(label(k), Data_List.fromFoldable(dictFoldable1)(edges(k))));
                            })(ks));
                        };
                    };
                };
            };
        };
    };
};
var topologicalSort = function (dictOrd) {
    return function (v) {
        var visit = function (__copy_state) {
            return function (__copy_stack) {
                var __tco_state = __copy_state;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(state, stack) {
                    var v1 = Data_CatList.uncons(stack);
                    if (v1 instanceof Data_Maybe.Nothing) {
                        __tco_done = true;
                        return state;
                    };
                    if (v1 instanceof Data_Maybe.Just && v1.value0.value0 instanceof Emit) {
                        var state$prime = {
                            result: new Data_List_Types.Cons(v1.value0.value0.value0, state.result), 
                            unvisited: state.unvisited
                        };
                        __tco_state = state$prime;
                        __copy_stack = v1.value0.value1;
                        return;
                    };
                    if (v1 instanceof Data_Maybe.Just && v1.value0.value0 instanceof Visit) {
                        if (Data_Map.member(dictOrd)(v1.value0.value0.value0)(state.unvisited)) {
                            var start = {
                                result: state.result, 
                                unvisited: Data_Map["delete"](dictOrd)(v1.value0.value0.value0)(state.unvisited)
                            };
                            var next = Data_Maybe.maybe(Data_Monoid.mempty(Data_List_Types.monoidList))(Data_Tuple.snd)(Data_Map.lookup(dictOrd)(v1.value0.value0.value0)(v));
                            __tco_state = start;
                            __copy_stack = Data_Semigroup.append(Data_CatList.semigroupCatList)(Data_CatList.fromFoldable(Data_List_Types.foldableList)(Data_Functor.map(Data_List_Types.functorList)(Visit.create)(next)))(Data_CatList.cons(new Emit(v1.value0.value0.value0))(v1.value0.value1));
                            return;
                        };
                        if (Data_Boolean.otherwise) {
                            __tco_state = state;
                            __copy_stack = v1.value0.value1;
                            return;
                        };
                    };
                    throw new Error("Failed pattern match at Data.Graph line 92, column 7 - line 110, column 40: " + [ v1.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_state, __copy_stack);
                };
                return __tco_result;
            };
        };
        var initialState = {
            unvisited: v, 
            result: Data_List_Types.Nil.value
        };
        var go = function (__copy_v1) {
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v1) {
                var v2 = Data_Map.findMin(v1.unvisited);
                if (v2 instanceof Data_Maybe.Just) {
                    __copy_v1 = visit(v1)(Data_CatList.fromFoldable(Data_Foldable.foldableArray)([ new Visit(v2.value0.key) ]));
                    return;
                };
                if (v2 instanceof Data_Maybe.Nothing) {
                    __tco_done = true;
                    return v1.result;
                };
                throw new Error("Failed pattern match at Data.Graph line 86, column 7 - line 88, column 26: " + [ v2.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__copy_v1);
            };
            return __tco_result;
        };
        return go(initialState);
    };
};
var outEdges = function (dictOrd) {
    return function (k) {
        return function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.snd)(Data_Map.lookup(dictOrd)(k)(v));
        };
    };
};
var lookup = function (dictOrd) {
    return function (k) {
        return function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.fst)(Data_Map.lookup(dictOrd)(k)(v));
        };
    };
};
var functorGraph = new Data_Functor.Functor(function (f) {
    return function (v) {
        return Data_Functor.map(Data_Map.functorMap)(Data_Bifunctor.lmap(Data_Tuple.bifunctorTuple)(f))(v);
    };
});
var fromMap = Graph;
module.exports = {
    fromMap: fromMap, 
    lookup: lookup, 
    outEdges: outEdges, 
    topologicalSort: topologicalSort, 
    unfoldGraph: unfoldGraph, 
    vertices: vertices, 
    functorGraph: functorGraph
};

},{"../Data.Bifunctor":61,"../Data.Boolean":63,"../Data.CatList":67,"../Data.Foldable":83,"../Data.Functor":91,"../Data.List":105,"../Data.List.Types":104,"../Data.Map":106,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Semigroup":128,"../Data.Tuple":145,"../Prelude":171}],95:[function(require,module,exports){
"use strict";

exports.boolConj = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolDisj = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

},{}],96:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var HeytingAlgebra = function (conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
};
var tt = function (dict) {
    return dict.tt;
};
var not = function (dict) {
    return dict.not;
};
var implies = function (dict) {
    return dict.implies;
};
var heytingAlgebraUnit = new HeytingAlgebra(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return Data_Unit.unit;
}, Data_Unit.unit);
var ff = function (dict) {
    return dict.ff;
};
var disj = function (dict) {
    return dict.disj;
};
var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
}, $foreign.boolNot, true);
var conj = function (dict) {
    return dict.conj;
};
var heytingAlgebraFunction = function (dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
        return function (g) {
            return function (a) {
                return conj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (g) {
            return function (a) {
                return disj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (v) {
        return ff(dictHeytingAlgebra);
    }, function (f) {
        return function (g) {
            return function (a) {
                return implies(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (a) {
            return not(dictHeytingAlgebra)(f(a));
        };
    }, function (v) {
        return tt(dictHeytingAlgebra);
    });
};
module.exports = {
    HeytingAlgebra: HeytingAlgebra, 
    conj: conj, 
    disj: disj, 
    ff: ff, 
    implies: implies, 
    not: not, 
    tt: tt, 
    heytingAlgebraBoolean: heytingAlgebraBoolean, 
    heytingAlgebraUnit: heytingAlgebraUnit, 
    heytingAlgebraFunction: heytingAlgebraFunction
};

},{"../Data.Unit":149,"./foreign":95}],97:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad = require("../Control.Monad");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Field = require("../Data.Field");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Identity = function (x) {
    return x;
};
var showIdentity = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Identity " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringIdentity = function (dictSemiring) {
    return dictSemiring;
};
var semigroupIdenity = function (dictSemigroup) {
    return dictSemigroup;
};
var ringIdentity = function (dictRing) {
    return dictRing;
};
var ordIdentity = function (dictOrd) {
    return dictOrd;
};
var newtypeIdentity = new Data_Newtype.Newtype(function (n) {
    return n;
}, Identity);
var monoidIdentity = function (dictMonoid) {
    return dictMonoid;
};
var lazyIdentity = function (dictLazy) {
    return dictLazy;
};
var heytingAlgebraIdentity = function (dictHeytingAlgebra) {
    return dictHeytingAlgebra;
};
var functorIdentity = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var invariantIdentity = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorIdentity));
var foldableIdentity = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var traversableIdentity = new Data_Traversable.Traversable(function () {
    return foldableIdentity;
}, function () {
    return functorIdentity;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Identity)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Identity)(f(v));
        };
    };
});
var fieldIdentity = function (dictField) {
    return dictField;
};
var extendIdentity = new Control_Extend.Extend(function () {
    return functorIdentity;
}, function (f) {
    return function (m) {
        return f(m);
    };
});
var euclideanRingIdentity = function (dictEuclideanRing) {
    return dictEuclideanRing;
};
var eqIdentity = function (dictEq) {
    return dictEq;
};
var eq1Identity = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqIdentity(dictEq));
});
var ord1Identity = new Data_Ord.Ord1(function () {
    return eq1Identity;
}, function (dictOrd) {
    return Data_Ord.compare(ordIdentity(dictOrd));
});
var comonadIdentity = new Control_Comonad.Comonad(function () {
    return extendIdentity;
}, function (v) {
    return v;
});
var commutativeRingIdentity = function (dictCommutativeRing) {
    return dictCommutativeRing;
};
var boundedIdentity = function (dictBounded) {
    return dictBounded;
};
var booleanAlgebraIdentity = function (dictBooleanAlgebra) {
    return dictBooleanAlgebra;
};
var applyIdentity = new Control_Apply.Apply(function () {
    return functorIdentity;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindIdentity = new Control_Bind.Bind(function () {
    return applyIdentity;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeIdentity = new Control_Applicative.Applicative(function () {
    return applyIdentity;
}, Identity);
var monadIdentity = new Control_Monad.Monad(function () {
    return applicativeIdentity;
}, function () {
    return bindIdentity;
});
var altIdentity = new Control_Alt.Alt(function () {
    return functorIdentity;
}, function (x) {
    return function (v) {
        return x;
    };
});
module.exports = {
    Identity: Identity, 
    newtypeIdentity: newtypeIdentity, 
    eqIdentity: eqIdentity, 
    ordIdentity: ordIdentity, 
    boundedIdentity: boundedIdentity, 
    heytingAlgebraIdentity: heytingAlgebraIdentity, 
    booleanAlgebraIdentity: booleanAlgebraIdentity, 
    semigroupIdenity: semigroupIdenity, 
    monoidIdentity: monoidIdentity, 
    semiringIdentity: semiringIdentity, 
    euclideanRingIdentity: euclideanRingIdentity, 
    ringIdentity: ringIdentity, 
    commutativeRingIdentity: commutativeRingIdentity, 
    fieldIdentity: fieldIdentity, 
    lazyIdentity: lazyIdentity, 
    showIdentity: showIdentity, 
    eq1Identity: eq1Identity, 
    ord1Identity: ord1Identity, 
    functorIdentity: functorIdentity, 
    invariantIdentity: invariantIdentity, 
    altIdentity: altIdentity, 
    applyIdentity: applyIdentity, 
    applicativeIdentity: applicativeIdentity, 
    bindIdentity: bindIdentity, 
    monadIdentity: monadIdentity, 
    extendIdentity: extendIdentity, 
    comonadIdentity: comonadIdentity, 
    foldableIdentity: foldableIdentity, 
    traversableIdentity: traversableIdentity
};

},{"../Control.Alt":4,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Lazy":16,"../Control.Monad":38,"../Data.BooleanAlgebra":64,"../Data.Bounded":66,"../Data.CommutativeRing":71,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Field":81,"../Data.Foldable":83,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.HeytingAlgebra":96,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Traversable":144,"../Prelude":171}],98:[function(require,module,exports){
"use strict";

// module Data.Int.Bits

exports.and = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 & n2;
  };
};

exports.or = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 | n2;
  };
};

exports.xor = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 ^ n2;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 << n2;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >> n2;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >>> n2;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
  return ~n;
};

},{}],99:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    and: $foreign.and, 
    complement: $foreign.complement, 
    or: $foreign.or, 
    shl: $foreign.shl, 
    shr: $foreign.shr, 
    xor: $foreign.xor, 
    zshr: $foreign.zshr
};

},{"./foreign":98}],100:[function(require,module,exports){
"use strict";

// module Data.Int

exports.fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

exports.toNumber = function (n) {
  return n;
};

exports.fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        /* jshint bitwise: false */
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.toStringAs = function (radix) {
  return function (i) {
    return i.toString(radix);
  };
};

exports.pow = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return Math.pow(x,y) | 0;
  };
};

},{}],101:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int_Bits = require("../Data.Int.Bits");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Global = require("../Global");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Radix = function (x) {
    return x;
};
var radix = function (n) {
    if (n >= 2 && n <= 36) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Int line 124, column 1 - line 125, column 38: " + [ n.constructor.name ]);
};
var odd = function (x) {
    return (x & 1) !== 0;
};
var octal = 8;
var hexadecimal = 16;
var fromStringAs = $foreign.fromStringAsImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromString = fromStringAs(10);
var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeClamp = function (x) {
    if (x === Global.infinity) {
        return 0;
    };
    if (x === -Global.infinity) {
        return 0;
    };
    if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
        return Data_Bounded.top(Data_Bounded.boundedInt);
    };
    if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
        return Data_Bounded.bottom(Data_Bounded.boundedInt);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.fromMaybe(0)(fromNumber(x));
    };
    throw new Error("Failed pattern match at Data.Int line 63, column 1 - line 68, column 43: " + [ x.constructor.name ]);
};
var round = function ($2) {
    return unsafeClamp($$Math.round($2));
};
var floor = function ($3) {
    return unsafeClamp($$Math.floor($3));
};
var even = function (x) {
    return (x & 1) === 0;
};
var decimal = 10;
var ceil = function ($4) {
    return unsafeClamp($$Math.ceil($4));
};
var binary = 2;
var base36 = 36;
module.exports = {
    base36: base36, 
    binary: binary, 
    ceil: ceil, 
    decimal: decimal, 
    even: even, 
    floor: floor, 
    fromNumber: fromNumber, 
    fromString: fromString, 
    fromStringAs: fromStringAs, 
    hexadecimal: hexadecimal, 
    octal: octal, 
    odd: odd, 
    radix: radix, 
    round: round, 
    pow: $foreign.pow, 
    toNumber: $foreign.toNumber, 
    toStringAs: $foreign.toStringAs
};

},{"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Bounded":66,"../Data.Eq":78,"../Data.HeytingAlgebra":96,"../Data.Int.Bits":99,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ring":126,"../Global":155,"../Math":166,"../Prelude":171,"./foreign":100}],102:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Prelude = require("../Prelude");
var uncons = function (v) {
    return {
        head: v.value0, 
        tail: v.value1
    };
};
var toList = function (v) {
    return new Data_List_Types.Cons(v.value0, v.value1);
};
var toUnfoldable = function (dictUnfoldable) {
    return function ($41) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
                return new Data_Tuple.Tuple(rec.head, rec.tail);
            })(Data_List.uncons(xs));
        })(toList($41));
    };
};
var tail = function (v) {
    return v.value1;
};
var singleton = function ($42) {
    return Data_List_Types.NonEmptyList(Data_NonEmpty.singleton(Data_List_Types.plusList)($42));
};
var length = function (v) {
    return 1 + Data_List.length(v.value1) | 0;
};
var last = function (v) {
    return Data_Maybe.fromMaybe(v.value0)(Data_List.last(v.value1));
};
var init = function (v) {
    return Data_Maybe.maybe(Data_List_Types.Nil.value)(function (v1) {
        return new Data_List_Types.Cons(v.value0, v1);
    })(Data_List.init(v.value1));
};
var head = function (v) {
    return v.value0;
};
var fromList = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(v.value0, v.value1));
    };
    throw new Error("Failed pattern match at Data.List.NonEmpty line 38, column 1 - line 38, column 25: " + [ v.constructor.name ]);
};
var fromFoldable = function (dictFoldable) {
    return function ($43) {
        return fromList(Data_List.fromFoldable(dictFoldable)($43));
    };
};
var concatMap = Data_Function.flip(Control_Bind.bind(Data_List_Types.bindNonEmptyList));
var appendFoldable = function (dictFoldable) {
    return function (v) {
        return function (ys) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(Data_List_Types.semigroupList)(v.value1)(Data_List.fromFoldable(dictFoldable)(ys)));
        };
    };
};
module.exports = {
    appendFoldable: appendFoldable, 
    concatMap: concatMap, 
    fromFoldable: fromFoldable, 
    fromList: fromList, 
    head: head, 
    init: init, 
    last: last, 
    length: length, 
    singleton: singleton, 
    tail: tail, 
    toList: toList, 
    toUnfoldable: toUnfoldable, 
    uncons: uncons
};

},{"../Control.Bind":12,"../Control.Semigroupoid":42,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.NonEmpty":119,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Prelude":171}],103:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Semigroup = require("../Data.Semigroup");
var Prelude = require("../Prelude");
var toList = function (v) {
    return new Data_List_Types.Cons(v.value0, new Data_List_Types.Cons(v.value1.value0, new Data_List_Types.Cons(v.value1.value1.value0, v.value1.value1.value1)));
};
var reverse = function (v) {
    var v1 = Data_List.reverse(v.value1.value1.value1);
    if (v1 instanceof Data_List_Types.Nil) {
        return new Data_NonEmpty.NonEmpty(v.value1.value1.value0, new Data_NonEmpty.NonEmpty(v.value1.value0, new Data_NonEmpty.NonEmpty(v.value0, Data_List_Types.Nil.value)));
    };
    if (v1 instanceof Data_List_Types.Cons && v1.value1 instanceof Data_List_Types.Nil) {
        return new Data_NonEmpty.NonEmpty(v1.value0, new Data_NonEmpty.NonEmpty(v.value1.value1.value0, new Data_NonEmpty.NonEmpty(v.value1.value0, new Data_List_Types.Cons(v.value0, Data_List_Types.Nil.value))));
    };
    if (v1 instanceof Data_List_Types.Cons && (v1.value1 instanceof Data_List_Types.Cons && v1.value1.value1 instanceof Data_List_Types.Nil)) {
        return new Data_NonEmpty.NonEmpty(v1.value0, new Data_NonEmpty.NonEmpty(v1.value1.value0, new Data_NonEmpty.NonEmpty(v.value1.value1.value0, new Data_List_Types.Cons(v.value1.value0, new Data_List_Types.Cons(v.value0, Data_List_Types.Nil.value)))));
    };
    if (v1 instanceof Data_List_Types.Cons && (v1.value1 instanceof Data_List_Types.Cons && v1.value1.value1 instanceof Data_List_Types.Cons)) {
        return new Data_NonEmpty.NonEmpty(v1.value0, new Data_NonEmpty.NonEmpty(v1.value1.value0, new Data_NonEmpty.NonEmpty(v1.value1.value1.value0, Data_Semigroup.append(Data_List_Types.semigroupList)(v1.value1.value1.value1)(new Data_List_Types.Cons(v.value1.value1.value0, new Data_List_Types.Cons(v.value1.value0, new Data_List_Types.Cons(v.value0, Data_List_Types.Nil.value)))))));
    };
    throw new Error("Failed pattern match at Data.List.ThreeOrMore line 25, column 3 - line 29, column 64: " + [ v1.constructor.name ]);
};
var last = function (v) {
    return Data_Maybe.fromMaybe(v.value1.value1.value0)(Data_List.last(v.value1.value1.value1));
};
module.exports = {
    last: last, 
    reverse: reverse, 
    toList: toList
};

},{"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.NonEmpty":119,"../Data.Semigroup":128,"../Prelude":171}],104:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Prelude = require("../Prelude");
var Nil = (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var NonEmptyList = function (x) {
    return x;
};
var toList = function (v) {
    return new Cons(v.value0, v.value1);
};
var newtypeNonEmptyList = new Data_Newtype.Newtype(function (n) {
    return n;
}, NonEmptyList);
var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldl(foldableList)(function (acc) {
            return function ($116) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f($116));
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, function (f) {
    var go = function (__copy_b) {
        return function (__copy_v) {
            var __tco_b = __copy_b;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(b, v) {
                if (v instanceof Nil) {
                    __tco_done = true;
                    return b;
                };
                if (v instanceof Cons) {
                    __tco_b = f(b)(v.value0);
                    __copy_v = v.value1;
                    return;
                };
                throw new Error("Failed pattern match at Data.List.Types line 76, column 12 - line 78, column 30: " + [ v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_b, __copy_v);
            };
            return __tco_result;
        };
    };
    return go;
}, function (f) {
    return function (b) {
        var rev = function (__copy_acc) {
            return function (__copy_v) {
                var __tco_acc = __copy_acc;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(acc, v) {
                    if (v instanceof Nil) {
                        __tco_done = true;
                        return acc;
                    };
                    if (v instanceof Cons) {
                        __tco_acc = new Cons(v.value0, acc);
                        __copy_v = v.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List.Types line 71, column 15 - line 73, column 33: " + [ v.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_acc, __copy_v);
                };
                return __tco_result;
            };
        };
        return function ($117) {
            return Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b)(rev(Nil.value)($117));
        };
    };
});
var foldableNonEmptyList = Data_NonEmpty.foldableNonEmpty(foldableList);
var functorList = new Data_Functor.Functor(function (f) {
    return Data_Foldable.foldr(foldableList)(function (x) {
        return function (acc) {
            return new Cons(f(x), acc);
        };
    })(Nil.value);
});
var functorNonEmptyList = Data_NonEmpty.functorNonEmpty(functorList);
var semigroupList = new Data_Semigroup.Semigroup(function (xs) {
    return function (ys) {
        return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
    };
});
var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
}, Nil.value);
var semigroupNonEmptyList = new Data_Semigroup.Semigroup(function (v) {
    return function (as$prime) {
        return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(semigroupList)(v.value1)(toList(as$prime)));
    };
});
var showList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Nil) {
            return "Nil";
        };
        return "(" + (Data_Foldable.intercalate(foldableList)(Data_Monoid.monoidString)(" : ")(Data_Functor.map(functorList)(Data_Show.show(dictShow))(v)) + " : Nil)");
    });
};
var showNonEmptyList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(NonEmptyList " + (Data_Show.show(Data_NonEmpty.showNonEmpty(dictShow)(showList(dictShow)))(v) + ")");
    });
};
var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
}, function () {
    return functorList;
}, function (dictApplicative) {
    return Data_Traversable.traverse(traversableList)(dictApplicative)(Control_Category.id(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function (f) {
        return function ($118) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value))(Data_Foldable.foldl(foldableList)(function (acc) {
                return function ($119) {
                    return Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc)(f($119));
                };
            })(Control_Applicative.pure(dictApplicative)(Nil.value))($118));
        };
    };
});
var traversableNonEmptyList = Data_NonEmpty.traversableNonEmpty(traversableList);
var unfoldableList = new Data_Unfoldable.Unfoldable(function (f) {
    return function (b) {
        var go = function (__copy_source) {
            return function (__copy_memo) {
                var __tco_source = __copy_source;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(source, memo) {
                    var v = f(source);
                    if (v instanceof Data_Maybe.Nothing) {
                        __tco_done = true;
                        return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(memo);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        __tco_source = v.value0.value1;
                        __copy_memo = new Cons(v.value0.value0, memo);
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List.Types line 84, column 22 - line 86, column 52: " + [ v.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_source, __copy_memo);
                };
                return __tco_result;
            };
        };
        return go(b)(Nil.value);
    };
});
var extendNonEmptyList = new Control_Extend.Extend(function () {
    return functorNonEmptyList;
}, function (f) {
    return function (v) {
        var go = function (a) {
            return function (v1) {
                return {
                    val: new Cons(f(new Data_NonEmpty.NonEmpty(a, v1.acc)), v1.val), 
                    acc: new Cons(a, v1.acc)
                };
            };
        };
        return new Data_NonEmpty.NonEmpty(f(v), (Data_Foldable.foldr(foldableList)(go)({
            val: Nil.value, 
            acc: Nil.value
        })(v.value1)).val);
    };
});
var extendList = new Control_Extend.Extend(function () {
    return functorList;
}, function (f) {
    return function (v) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            var go = function (a$prime) {
                return function (v1) {
                    var acc$prime = new Cons(a$prime, v1.acc);
                    return {
                        val: new Cons(f(acc$prime), v1.val), 
                        acc: acc$prime
                    };
                };
            };
            return new Cons(f(v), (Data_Foldable.foldr(foldableList)(go)({
                val: Nil.value, 
                acc: Nil.value
            })(v.value1)).val);
        };
        throw new Error("Failed pattern match at Data.List.Types line 118, column 3 - line 118, column 21: " + [ f.constructor.name, v.constructor.name ]);
    };
});
var eq1List = new Data_Eq.Eq1(function (dictEq) {
    return function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_v2) {
                        var __tco_v = __copy_v;
                        var __tco_v1 = __copy_v1;
                        var __tco_done = false;
                        var __tco_result;
                        function __tco_loop(v, v1, v2) {
                            if (!v2) {
                                __tco_done = true;
                                return false;
                            };
                            if (v instanceof Nil && v1 instanceof Nil) {
                                __tco_done = true;
                                return v2;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                __tco_v = v.value1;
                                __tco_v1 = v1.value1;
                                __copy_v2 = v2 && Data_Eq.eq(dictEq)(v1.value0)(v.value0);
                                return;
                            };
                            __tco_done = true;
                            return false;
                        };
                        while (!__tco_done) {
                            __tco_result = __tco_loop(__tco_v, __tco_v1, __copy_v2);
                        };
                        return __tco_result;
                    };
                };
            };
            return go(xs)(ys)(true);
        };
    };
});
var eqList = function (dictEq) {
    return new Data_Eq.Eq(Data_Eq.eq1(eq1List)(dictEq));
};
var eqNonEmptyList = function (dictEq) {
    return Data_NonEmpty.eqNonEmpty(eq1List)(dictEq);
};
var ord1List = new Data_Ord.Ord1(function () {
    return eq1List;
}, function (dictOrd) {
    return function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    var __tco_v = __copy_v;
                    var __tco_done = false;
                    var __tco_result;
                    function __tco_loop(v, v1) {
                        if (v instanceof Nil && v1 instanceof Nil) {
                            __tco_done = true;
                            return Data_Ordering.EQ.value;
                        };
                        if (v instanceof Nil) {
                            __tco_done = true;
                            return Data_Ordering.LT.value;
                        };
                        if (v1 instanceof Nil) {
                            __tco_done = true;
                            return Data_Ordering.GT.value;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            var v2 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                            if (v2 instanceof Data_Ordering.EQ) {
                                __tco_v = v.value1;
                                __copy_v1 = v1.value1;
                                return;
                            };
                            __tco_done = true;
                            return v2;
                        };
                        throw new Error("Failed pattern match at Data.List.Types line 49, column 20 - line 57, column 23: " + [ v.constructor.name, v1.constructor.name ]);
                    };
                    while (!__tco_done) {
                        __tco_result = __tco_loop(__tco_v, __copy_v1);
                    };
                    return __tco_result;
                };
            };
            return go(xs)(ys);
        };
    };
});
var ordList = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqList(dictOrd.Eq0());
    }, Data_Ord.compare1(ord1List)(dictOrd));
};
var ordNonEmptyList = function (dictOrd) {
    return Data_NonEmpty.ordNonEmpty(ord1List)(dictOrd);
};
var comonadNonEmptyList = new Control_Comonad.Comonad(function () {
    return extendNonEmptyList;
}, function (v) {
    return v.value0;
});
var applyList = new Control_Apply.Apply(function () {
    return functorList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List.Types line 93, column 3 - line 93, column 20: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var applyNonEmptyList = new Control_Apply.Apply(function () {
    return functorNonEmptyList;
}, function (v) {
    return function (v1) {
        return new Data_NonEmpty.NonEmpty(v.value0(v1.value0), Data_Semigroup.append(semigroupList)(Control_Apply.apply(applyList)(v.value1)(new Cons(v1.value0, Nil.value)))(Control_Apply.apply(applyList)(new Cons(v.value0, v.value1))(v1.value1)));
    };
});
var bindList = new Control_Bind.Bind(function () {
    return applyList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(v1(v.value0))(Control_Bind.bind(bindList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List.Types line 100, column 3 - line 100, column 19: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindNonEmptyList = new Control_Bind.Bind(function () {
    return applyNonEmptyList;
}, function (v) {
    return function (f) {
        var v1 = f(v.value0);
        return new Data_NonEmpty.NonEmpty(v1.value0, Data_Semigroup.append(semigroupList)(v1.value1)(Control_Bind.bind(bindList)(v.value1)(function ($120) {
            return toList(f($120));
        })));
    };
});
var applicativeList = new Control_Applicative.Applicative(function () {
    return applyList;
}, function (a) {
    return new Cons(a, Nil.value);
});
var monadList = new Control_Monad.Monad(function () {
    return applicativeList;
}, function () {
    return bindList;
});
var altNonEmptyList = new Control_Alt.Alt(function () {
    return functorNonEmptyList;
}, Data_Semigroup.append(semigroupNonEmptyList));
var altList = new Control_Alt.Alt(function () {
    return functorList;
}, Data_Semigroup.append(semigroupList));
var plusList = new Control_Plus.Plus(function () {
    return altList;
}, Nil.value);
var alternativeList = new Control_Alternative.Alternative(function () {
    return applicativeList;
}, function () {
    return plusList;
});
var monadZeroList = new Control_MonadZero.MonadZero(function () {
    return alternativeList;
}, function () {
    return monadList;
});
var monadPlusList = new Control_MonadPlus.MonadPlus(function () {
    return monadZeroList;
});
var applicativeNonEmptyList = new Control_Applicative.Applicative(function () {
    return applyNonEmptyList;
}, function ($121) {
    return NonEmptyList(Data_NonEmpty.singleton(plusList)($121));
});
var monadNonEmptyList = new Control_Monad.Monad(function () {
    return applicativeNonEmptyList;
}, function () {
    return bindNonEmptyList;
});
module.exports = {
    Nil: Nil, 
    Cons: Cons, 
    NonEmptyList: NonEmptyList, 
    toList: toList, 
    showList: showList, 
    eqList: eqList, 
    eq1List: eq1List, 
    ordList: ordList, 
    ord1List: ord1List, 
    semigroupList: semigroupList, 
    monoidList: monoidList, 
    functorList: functorList, 
    foldableList: foldableList, 
    unfoldableList: unfoldableList, 
    traversableList: traversableList, 
    applyList: applyList, 
    applicativeList: applicativeList, 
    bindList: bindList, 
    monadList: monadList, 
    altList: altList, 
    plusList: plusList, 
    alternativeList: alternativeList, 
    monadZeroList: monadZeroList, 
    monadPlusList: monadPlusList, 
    extendList: extendList, 
    newtypeNonEmptyList: newtypeNonEmptyList, 
    eqNonEmptyList: eqNonEmptyList, 
    ordNonEmptyList: ordNonEmptyList, 
    showNonEmptyList: showNonEmptyList, 
    functorNonEmptyList: functorNonEmptyList, 
    applyNonEmptyList: applyNonEmptyList, 
    applicativeNonEmptyList: applicativeNonEmptyList, 
    bindNonEmptyList: bindNonEmptyList, 
    monadNonEmptyList: monadNonEmptyList, 
    altNonEmptyList: altNonEmptyList, 
    extendNonEmptyList: extendNonEmptyList, 
    comonadNonEmptyList: comonadNonEmptyList, 
    semigroupNonEmptyList: semigroupNonEmptyList, 
    foldableNonEmptyList: foldableNonEmptyList, 
    traversableNonEmptyList: traversableNonEmptyList
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Control.MonadPlus":39,"../Control.MonadZero":40,"../Control.Plus":41,"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.NonEmpty":119,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Prelude":171}],105:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var updateAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Data_List_Types.Cons) {
                return new Data_Maybe.Just(new Data_List_Types.Cons(v1, v2.value1));
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(updateAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var unzip = Data_Foldable.foldr(Data_List_Types.foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(new Data_List_Types.Cons(v.value0, v1.value0), new Data_List_Types.Cons(v.value1, v1.value1));
    };
})(new Data_Tuple.Tuple(Data_List_Types.Nil.value, Data_List_Types.Nil.value));
var uncons = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just({
            head: v.value0, 
            tail: v.value1
        });
    };
    throw new Error("Failed pattern match at Data.List line 253, column 1 - line 253, column 21: " + [ v.constructor.name ]);
};
var toUnfoldable = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var tail = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(v.value1);
    };
    throw new Error("Failed pattern match at Data.List line 239, column 1 - line 239, column 19: " + [ v.constructor.name ]);
};
var span = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Cons && v(v1.value0)) {
            var v2 = span(v)(v1.value1);
            return {
                init: new Data_List_Types.Cons(v1.value0, v2.init), 
                rest: v2.rest
            };
        };
        return {
            init: Data_List_Types.Nil.value, 
            rest: v1
        };
    };
};
var singleton = function (a) {
    return new Data_List_Types.Cons(a, Data_List_Types.Nil.value);
};
var sortBy = function (cmp) {
    var merge = function (v) {
        return function (v1) {
            if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1.value0))(Data_Ordering.GT.value)) {
                    return new Data_List_Types.Cons(v1.value0, merge(v)(v1.value1));
                };
                if (Data_Boolean.otherwise) {
                    return new Data_List_Types.Cons(v.value0, merge(v.value1)(v1));
                };
            };
            if (v instanceof Data_List_Types.Nil) {
                return v1;
            };
            if (v1 instanceof Data_List_Types.Nil) {
                return v;
            };
            throw new Error("Failed pattern match at Data.List line 468, column 3 - line 470, column 41: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
    var mergePairs = function (v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
            return new Data_List_Types.Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
        };
        return v;
    };
    var mergeAll = function (__copy_v) {
        var __tco_done = false;
        var __tco_result;
        function __tco_loop(v) {
            if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                __tco_done = true;
                return v.value0;
            };
            __copy_v = mergePairs(v);
            return;
        };
        while (!__tco_done) {
            __tco_result = __tco_loop(__copy_v);
        };
        return __tco_result;
    };
    var sequences = function (v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
            if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v.value1.value0))(Data_Ordering.GT.value)) {
                return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
            };
            if (Data_Boolean.otherwise) {
                return ascending(v.value1.value0)(function (v1) {
                    return new Data_List_Types.Cons(v.value0, v1);
                })(v.value1.value1);
            };
        };
        return singleton(v);
    };
    var descending = function (__copy_a) {
        return function (__copy_as) {
            return function (__copy_v) {
                var __tco_a = __copy_a;
                var __tco_as = __copy_as;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(a, as, v) {
                    if (v instanceof Data_List_Types.Cons && Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                        __tco_a = v.value0;
                        __tco_as = new Data_List_Types.Cons(a, as);
                        __copy_v = v.value1;
                        return;
                    };
                    __tco_done = true;
                    return new Data_List_Types.Cons(new Data_List_Types.Cons(a, as), sequences(v));
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_a, __tco_as, __copy_v);
                };
                return __tco_result;
            };
        };
    };
    var ascending = function (__copy_a) {
        return function (__copy_as) {
            return function (__copy_v) {
                var __tco_a = __copy_a;
                var __tco_as = __copy_as;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(a, as, v) {
                    if (v instanceof Data_List_Types.Cons && Data_Eq.notEq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                        __tco_a = v.value0;
                        __tco_as = function (ys) {
                            return as(new Data_List_Types.Cons(a, ys));
                        };
                        __copy_v = v.value1;
                        return;
                    };
                    __tco_done = true;
                    return new Data_List_Types.Cons(as(singleton(a)), sequences(v));
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_a, __tco_as, __copy_v);
                };
                return __tco_result;
            };
        };
    };
    return function ($304) {
        return mergeAll(sequences($304));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var reverse = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var __tco_acc = __copy_acc;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return acc;
                };
                if (v instanceof Data_List_Types.Cons) {
                    __tco_acc = new Data_List_Types.Cons(v.value0, acc);
                    __copy_v = v.value1;
                    return;
                };
                throw new Error("Failed pattern match at Data.List line 359, column 11 - line 362, column 36: " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_acc, __copy_v);
            };
            return __tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
})();
var snoc = function (xs) {
    return function (x) {
        return reverse(new Data_List_Types.Cons(x, reverse(xs)));
    };
};
var take = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var __tco_acc = __copy_acc;
                var __tco_v = __copy_v;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(acc, v, v1) {
                    if (v === 0) {
                        __tco_done = true;
                        return reverse(acc);
                    };
                    if (v1 instanceof Data_List_Types.Nil) {
                        __tco_done = true;
                        return reverse(acc);
                    };
                    if (v1 instanceof Data_List_Types.Cons) {
                        __tco_acc = new Data_List_Types.Cons(v1.value0, acc);
                        __tco_v = v - 1 | 0;
                        __copy_v1 = v1.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List line 486, column 8 - line 490, column 46: " + [ acc.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_acc, __tco_v, __copy_v1);
                };
                return __tco_result;
            };
        };
    };
    return go(Data_List_Types.Nil.value);
})();
var takeWhile = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var __tco_acc = __copy_acc;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Cons && p(v.value0)) {
                    __tco_acc = new Data_List_Types.Cons(v.value0, acc);
                    __copy_v = v.value1;
                    return;
                };
                __tco_done = true;
                return reverse(acc);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_acc, __copy_v);
            };
            return __tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var unsnoc = function (lst) {
    var go = function (__copy_v) {
        return function (__copy_acc) {
            var __tco_v = __copy_v;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v, acc) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return new Data_Maybe.Just({
                        revInit: acc, 
                        last: v.value0
                    });
                };
                if (v instanceof Data_List_Types.Cons) {
                    __tco_v = v.value1;
                    __copy_acc = new Data_List_Types.Cons(v.value0, acc);
                    return;
                };
                throw new Error("Failed pattern match at Data.List line 261, column 14 - line 265, column 36: " + [ v.constructor.name, acc.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_v, __copy_acc);
            };
            return __tco_result;
        };
    };
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (h) {
        return {
            init: reverse(h.revInit), 
            last: h.last
        };
    })(go(lst)(Data_List_Types.Nil.value));
};
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_acc) {
                        var __tco_v = __copy_v;
                        var __tco_v1 = __copy_v1;
                        var __tco_done = false;
                        var __tco_result;
                        function __tco_loop(v, v1, acc) {
                            if (v instanceof Data_List_Types.Nil) {
                                __tco_done = true;
                                return acc;
                            };
                            if (v1 instanceof Data_List_Types.Nil) {
                                __tco_done = true;
                                return acc;
                            };
                            if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                                __tco_v = v.value1;
                                __tco_v1 = v1.value1;
                                __copy_acc = new Data_List_Types.Cons(f(v.value0)(v1.value0), acc);
                                return;
                            };
                            throw new Error("Failed pattern match at Data.List line 650, column 19 - line 654, column 52: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                        };
                        while (!__tco_done) {
                            __tco_result = __tco_loop(__tco_v, __tco_v1, __copy_acc);
                        };
                        return __tco_result;
                    };
                };
            };
            return reverse(go(xs)(ys)(Data_List_Types.Nil.value));
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_List_Types.traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var range = function (start) {
    return function (end) {
        if (start === end) {
            return singleton(start);
        };
        if (Data_Boolean.otherwise) {
            var go = function (__copy_s) {
                return function (__copy_e) {
                    return function (__copy_step) {
                        return function (__copy_rest) {
                            var __tco_s = __copy_s;
                            var __tco_e = __copy_e;
                            var __tco_step = __copy_step;
                            var __tco_done = false;
                            var __tco_result;
                            function __tco_loop(s, e, step, rest) {
                                if (s === e) {
                                    __tco_done = true;
                                    return new Data_List_Types.Cons(s, rest);
                                };
                                if (Data_Boolean.otherwise) {
                                    __tco_s = s + step | 0;
                                    __tco_e = e;
                                    __tco_step = step;
                                    __copy_rest = new Data_List_Types.Cons(s, rest);
                                    return;
                                };
                                throw new Error("Failed pattern match at Data.List line 139, column 31 - line 142, column 65: " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                            };
                            while (!__tco_done) {
                                __tco_result = __tco_loop(__tco_s, __tco_e, __tco_step, __copy_rest);
                            };
                            return __tco_result;
                        };
                    };
                };
            };
            return go(end)(start)((function () {
                var $195 = start > end;
                if ($195) {
                    return 1;
                };
                return -1 | 0;
            })())(Data_List_Types.Nil.value);
        };
        throw new Error("Failed pattern match at Data.List line 138, column 1 - line 142, column 65: " + [ start.constructor.name, end.constructor.name ]);
    };
};
var $$null = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return true;
    };
    return false;
};
var mapWithIndex = function (f) {
    return function (lst) {
        var go = function (__copy_v) {
            return function (__copy_v1) {
                return function (__copy_acc) {
                    var __tco_v = __copy_v;
                    var __tco_v1 = __copy_v1;
                    var __tco_done = false;
                    var __tco_result;
                    function __tco_loop(v, v1, acc) {
                        if (v1 instanceof Data_List_Types.Nil) {
                            __tco_done = true;
                            return acc;
                        };
                        if (v1 instanceof Data_List_Types.Cons) {
                            __tco_v = v + 1 | 0;
                            __tco_v1 = v1.value1;
                            __copy_acc = new Data_List_Types.Cons(f(v)(v1.value0), acc);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.List line 424, column 22 - line 427, column 48: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                    };
                    while (!__tco_done) {
                        __tco_result = __tco_loop(__tco_v, __tco_v1, __copy_acc);
                    };
                    return __tco_result;
                };
            };
        };
        return reverse(go(0)(lst)(Data_List_Types.Nil.value));
    };
};
var mapMaybe = function (f) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var __tco_acc = __copy_acc;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return reverse(acc);
                };
                if (v instanceof Data_List_Types.Cons) {
                    var v1 = f(v.value0);
                    if (v1 instanceof Data_Maybe.Nothing) {
                        __tco_acc = acc;
                        __copy_v = v.value1;
                        return;
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        __tco_acc = new Data_List_Types.Cons(v1.value0, acc);
                        __copy_v = v.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List line 412, column 5 - line 414, column 32: " + [ v1.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List line 408, column 14 - line 414, column 32: " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_acc, __copy_v);
            };
            return __tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var manyRec = function (dictMonadRec) {
    return function (dictAlternative) {
        return function (p) {
            var go = function (acc) {
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Control_Monad_Rec_Class.Loop.create)(p))(Control_Applicative.pure(dictAlternative.Applicative0())(new Control_Monad_Rec_Class.Done(Data_Unit.unit))))(function (v) {
                    return Control_Applicative.pure(dictAlternative.Applicative0())(Data_Bifunctor.bimap(Control_Monad_Rec_Class.bifunctorStep)(function (v1) {
                        return new Data_List_Types.Cons(v1, acc);
                    })(function (v1) {
                        return reverse(acc);
                    })(v));
                });
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go)(Data_List_Types.Nil.value);
        };
    };
};
var someRec = function (dictMonadRec) {
    return function (dictAlternative) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Data_List_Types.Cons.create)(v))(manyRec(dictMonadRec)(dictAlternative)(v));
        };
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Data_List_Types.Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())(Data_List_Types.Nil.value));
        };
    };
};
var length = Data_Foldable.foldl(Data_List_Types.foldableList)(function (acc) {
    return function (v) {
        return acc + 1 | 0;
    };
})(0);
var last = function (__copy_v) {
    var __tco_done = false;
    var __tco_result;
    function __tco_loop(v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
            __tco_done = true;
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof Data_List_Types.Cons) {
            __copy_v = v.value1;
            return;
        };
        __tco_done = true;
        return Data_Maybe.Nothing.value;
    };
    while (!__tco_done) {
        __tco_result = __tco_loop(__copy_v);
    };
    return __tco_result;
};
var insertBy = function (v) {
    return function (x) {
        return function (v1) {
            if (v1 instanceof Data_List_Types.Nil) {
                return singleton(x);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                var v2 = v(x)(v1.value0);
                if (v2 instanceof Data_Ordering.GT) {
                    return new Data_List_Types.Cons(v1.value0, insertBy(v)(x)(v1.value1));
                };
                return new Data_List_Types.Cons(x, v1);
            };
            throw new Error("Failed pattern match at Data.List line 210, column 1 - line 210, column 31: " + [ v.constructor.name, x.constructor.name, v1.constructor.name ]);
        };
    };
};
var insertAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0) {
                return new Data_Maybe.Just(new Data_List_Types.Cons(v1, v2));
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(insertAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var init = function (lst) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return v.init;
    })(unsnoc(lst));
};
var index = function (__copy_v) {
    return function (__copy_v1) {
        var __tco_v = __copy_v;
        var __tco_done = false;
        var __tco_result;
        function __tco_loop(v, v1) {
            if (v instanceof Data_List_Types.Nil) {
                __tco_done = true;
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_List_Types.Cons && v1 === 0) {
                __tco_done = true;
                return new Data_Maybe.Just(v.value0);
            };
            if (v instanceof Data_List_Types.Cons) {
                __tco_v = v.value1;
                __copy_v1 = v1 - 1 | 0;
                return;
            };
            throw new Error("Failed pattern match at Data.List line 275, column 1 - line 275, column 22: " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!__tco_done) {
            __tco_result = __tco_loop(__tco_v, __copy_v1);
        };
        return __tco_result;
    };
};
var head = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(v.value0);
    };
    throw new Error("Failed pattern match at Data.List line 224, column 1 - line 224, column 19: " + [ v.constructor.name ]);
};
var transpose = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_List_Types.Nil.value;
    };
    if (v instanceof Data_List_Types.Cons && v.value0 instanceof Data_List_Types.Nil) {
        return transpose(v.value1);
    };
    if (v instanceof Data_List_Types.Cons && v.value0 instanceof Data_List_Types.Cons) {
        return new Data_List_Types.Cons(new Data_List_Types.Cons(v.value0.value0, mapMaybe(head)(v.value1)), transpose(new Data_List_Types.Cons(v.value0.value1, mapMaybe(tail)(v.value1))));
    };
    throw new Error("Failed pattern match at Data.List line 687, column 1 - line 687, column 20: " + [ v.constructor.name ]);
};
var groupBy = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Nil) {
            return Data_List_Types.Nil.value;
        };
        if (v1 instanceof Data_List_Types.Cons) {
            var v2 = span(v(v1.value0))(v1.value1);
            return new Data_List_Types.Cons(new Data_NonEmpty.NonEmpty(v1.value0, v2.init), groupBy(v)(v2.rest));
        };
        throw new Error("Failed pattern match at Data.List line 560, column 1 - line 560, column 20: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var group = function (dictEq) {
    return groupBy(Data_Eq.eq(dictEq));
};
var group$prime = function (dictOrd) {
    return function ($305) {
        return group(dictOrd.Eq0())(sort(dictOrd)($305));
    };
};
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Data_List_Types.Cons.create)(Data_List_Types.Nil.value);
};
var foldM = function (dictMonad) {
    return function (v) {
        return function (a) {
            return function (v1) {
                if (v1 instanceof Data_List_Types.Nil) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(a);
                };
                if (v1 instanceof Data_List_Types.Cons) {
                    return Control_Bind.bind(dictMonad.Bind1())(v(a)(v1.value0))(function (a$prime) {
                        return foldM(dictMonad)(v)(a$prime)(v1.value1);
                    });
                };
                throw new Error("Failed pattern match at Data.List line 698, column 1 - line 698, column 23: " + [ v.constructor.name, a.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function (__copy_v) {
        return function (__copy_v1) {
            var __tco_v = __copy_v;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v, v1) {
                if (v1 instanceof Data_List_Types.Cons) {
                    if (fn(v1.value0)) {
                        __tco_done = true;
                        return new Data_Maybe.Just(v);
                    };
                    if (Data_Boolean.otherwise) {
                        __tco_v = v + 1 | 0;
                        __copy_v1 = v1.value1;
                        return;
                    };
                };
                if (v1 instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List line 295, column 3 - line 296, column 44: " + [ v.constructor.name, v1.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_v, __copy_v1);
            };
            return __tco_result;
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return (length(xs) - 1 | 0) - v | 0;
        })(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_List_Types.Nil) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_List_Types.Nil.value);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                return Control_Bind.bind(dictMonad.Bind1())(v(v1.value0))(function (v2) {
                    return Control_Bind.bind(dictMonad.Bind1())(filterM(dictMonad)(v)(v1.value1))(function (v3) {
                        return Control_Applicative.pure(dictMonad.Applicative0())((function () {
                            if (v2) {
                                return new Data_List_Types.Cons(v1.value0, v3);
                            };
                            return v3;
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List line 397, column 1 - line 397, column 25: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var filter = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var __tco_acc = __copy_acc;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return reverse(acc);
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (p(v.value0)) {
                        __tco_acc = new Data_List_Types.Cons(v.value0, acc);
                        __copy_v = v.value1;
                        return;
                    };
                    if (Data_Boolean.otherwise) {
                        __tco_acc = acc;
                        __copy_v = v.value1;
                        return;
                    };
                };
                throw new Error("Failed pattern match at Data.List line 381, column 12 - line 386, column 28: " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_acc, __copy_v);
            };
            return __tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var intersectBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            if (v2 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            return filter(function (x) {
                return Data_Foldable.any(Data_List_Types.foldableList)(Data_HeytingAlgebra.heytingAlgebraBoolean)(v(x))(v2);
            })(v1);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var nubBy = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Nil) {
            return Data_List_Types.Nil.value;
        };
        if (v1 instanceof Data_List_Types.Cons) {
            return new Data_List_Types.Cons(v1.value0, nubBy(v)(filter(function (y) {
                return !v(v1.value0)(y);
            })(v1.value1)));
        };
        throw new Error("Failed pattern match at Data.List line 579, column 1 - line 579, column 22: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    var go = function (__copy_v) {
        var __tco_done = false;
        var __tco_result;
        function __tco_loop(v) {
            if (v instanceof Data_List_Types.Cons && p(v.value0)) {
                __copy_v = v.value1;
                return;
            };
            __tco_done = true;
            return v;
        };
        while (!__tco_done) {
            __tco_result = __tco_loop(__copy_v);
        };
        return __tco_result;
    };
    return go;
};
var drop = function (__copy_v) {
    return function (__copy_v1) {
        var __tco_v = __copy_v;
        var __tco_done = false;
        var __tco_result;
        function __tco_loop(v, v1) {
            if (v === 0) {
                __tco_done = true;
                return v1;
            };
            if (v1 instanceof Data_List_Types.Nil) {
                __tco_done = true;
                return Data_List_Types.Nil.value;
            };
            if (v1 instanceof Data_List_Types.Cons) {
                __tco_v = v - 1 | 0;
                __copy_v1 = v1.value1;
                return;
            };
            throw new Error("Failed pattern match at Data.List line 505, column 1 - line 505, column 15: " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!__tco_done) {
            __tco_result = __tco_loop(__tco_v, __copy_v1);
        };
        return __tco_result;
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start | 0)(drop(start)(xs));
        };
    };
};
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            if (v2 instanceof Data_List_Types.Cons && v(v1)(v2.value0)) {
                return v2.value1;
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return new Data_List_Types.Cons(v2.value0, deleteBy(v)(v1)(v2.value1));
            };
            throw new Error("Failed pattern match at Data.List line 606, column 1 - line 606, column 23: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_List_Types.semigroupList)(xs)(Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var deleteAt = function (v) {
    return function (v1) {
        if (v === 0 && v1 instanceof Data_List_Types.Cons) {
            return new Data_Maybe.Just(v1.value1);
        };
        if (v1 instanceof Data_List_Types.Cons) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v2) {
                return new Data_List_Types.Cons(v1.value0, v2);
            })(deleteAt(v - 1 | 0)(v1.value1));
        };
        return Data_Maybe.Nothing.value;
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip($$delete(dictEq)));
};
var concatMap = Data_Function.flip(Control_Bind.bind(Data_List_Types.bindList));
var concat = function (v) {
    return Control_Bind.bind(Data_List_Types.bindList)(v)(Control_Category.id(Control_Category.categoryFn));
};
var catMaybes = mapMaybe(Control_Category.id(Control_Category.categoryFn));
var alterAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Data_List_Types.Cons) {
                return Data_Maybe.Just.create((function () {
                    var v3 = v1(v2.value0);
                    if (v3 instanceof Data_Maybe.Nothing) {
                        return v2.value1;
                    };
                    if (v3 instanceof Data_Maybe.Just) {
                        return new Data_List_Types.Cons(v3.value0, v2.value1);
                    };
                    throw new Error("Failed pattern match at Data.List line 345, column 3 - line 347, column 23: " + [ v3.constructor.name ]);
                })());
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(alterAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($306) {
            return Data_Maybe.Just.create(f($306));
        });
    };
};
module.exports = {
    alterAt: alterAt, 
    catMaybes: catMaybes, 
    concat: concat, 
    concatMap: concatMap, 
    "delete": $$delete, 
    deleteAt: deleteAt, 
    deleteBy: deleteBy, 
    difference: difference, 
    drop: drop, 
    dropWhile: dropWhile, 
    elemIndex: elemIndex, 
    elemLastIndex: elemLastIndex, 
    filter: filter, 
    filterM: filterM, 
    findIndex: findIndex, 
    findLastIndex: findLastIndex, 
    foldM: foldM, 
    fromFoldable: fromFoldable, 
    group: group, 
    "group'": group$prime, 
    groupBy: groupBy, 
    head: head, 
    index: index, 
    init: init, 
    insert: insert, 
    insertAt: insertAt, 
    insertBy: insertBy, 
    intersect: intersect, 
    intersectBy: intersectBy, 
    last: last, 
    length: length, 
    many: many, 
    manyRec: manyRec, 
    mapMaybe: mapMaybe, 
    mapWithIndex: mapWithIndex, 
    modifyAt: modifyAt, 
    nub: nub, 
    nubBy: nubBy, 
    "null": $$null, 
    range: range, 
    reverse: reverse, 
    singleton: singleton, 
    slice: slice, 
    snoc: snoc, 
    some: some, 
    someRec: someRec, 
    sort: sort, 
    sortBy: sortBy, 
    span: span, 
    tail: tail, 
    take: take, 
    takeWhile: takeWhile, 
    toUnfoldable: toUnfoldable, 
    transpose: transpose, 
    uncons: uncons, 
    union: union, 
    unionBy: unionBy, 
    unsnoc: unsnoc, 
    unzip: unzip, 
    updateAt: updateAt, 
    zip: zip, 
    zipWith: zipWith, 
    zipWithA: zipWithA
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Lazy":16,"../Control.Monad.Rec.Class":32,"../Control.Semigroupoid":42,"../Data.Bifunctor":61,"../Data.Boolean":63,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.NonEmpty":119,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Data.Unit":149,"../Prelude":171}],106:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Leaf = (function () {
    function Leaf() {

    };
    Leaf.value = new Leaf();
    return Leaf;
})();
var Two = (function () {
    function Two(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Two(value0, value1, value2, value3);
                };
            };
        };
    };
    return Two;
})();
var Three = (function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
        this.value6 = value6;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return function (value6) {
                                return new Three(value0, value1, value2, value3, value4, value5, value6);
                            };
                        };
                    };
                };
            };
        };
    };
    return Three;
})();
var TwoLeft = (function () {
    function TwoLeft(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoLeft(value0, value1, value2);
            };
        };
    };
    return TwoLeft;
})();
var TwoRight = (function () {
    function TwoRight(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoRight(value0, value1, value2);
            };
        };
    };
    return TwoRight;
})();
var ThreeLeft = (function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeLeft;
})();
var ThreeMiddle = (function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeMiddle;
})();
var ThreeRight = (function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeRight(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeRight;
})();
var KickUp = (function () {
    function KickUp(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new KickUp(value0, value1, value2, value3);
                };
            };
        };
    };
    return KickUp;
})();
var values = function (v) {
    if (v instanceof Leaf) {
        return Data_List_Types.Nil.value;
    };
    if (v instanceof Two) {
        return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(values(v.value3)));
    };
    if (v instanceof Three) {
        return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value5))(values(v.value6)))));
    };
    throw new Error("Failed pattern match at Data.Map line 425, column 1 - line 425, column 18: " + [ v.constructor.name ]);
};
var size = function ($673) {
    return Data_List.length(values($673));
};
var singleton = function (k) {
    return function (v) {
        return new Two(Leaf.value, k, v, Leaf.value);
    };
};
var toAscUnfoldable = function (dictUnfoldable) {
    return function (m) {
        var go = function (__copy_v) {
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof Leaf) {
                        __copy_v = v.value1;
                        return;
                    };
                    if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
                        __tco_done = true;
                        return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), v.value1));
                    };
                    if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
                        __tco_done = true;
                        return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                    };
                    if (v.value0 instanceof Two) {
                        __copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                        return;
                    };
                    if (v.value0 instanceof Three) {
                        __copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value6, v.value1)))));
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map line 406, column 18 - line 415, column 71: " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 404, column 21 - line 415, column 71: " + [ v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__copy_v);
            };
            return __tco_result;
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
    };
};
var toAscArray = toAscUnfoldable(Data_Unfoldable.unfoldableArray);
var toUnfoldable = function (dictUnfoldable) {
    return function (m) {
        var go = function (__copy_v) {
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof Leaf) {
                        __copy_v = v.value1;
                        return;
                    };
                    if (v.value0 instanceof Two) {
                        __tco_done = true;
                        return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(v.value0.value3, v.value1))));
                    };
                    if (v.value0 instanceof Three) {
                        __tco_done = true;
                        return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(v.value0.value6, v.value1))))));
                    };
                    throw new Error("Failed pattern match at Data.Map line 395, column 18 - line 400, column 77: " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 393, column 18 - line 400, column 77: " + [ v.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__copy_v);
            };
            return __tco_result;
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
    };
};
var showTree = function (dictShow) {
    return function (dictShow1) {
        return function (v) {
            if (v instanceof Leaf) {
                return "Leaf";
            };
            if (v instanceof Two) {
                return "Two (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Data_Show.show(dictShow)(v.value1) + (") (" + (Data_Show.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + ")")))))));
            };
            if (v instanceof Three) {
                return "Three (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Data_Show.show(dictShow)(v.value1) + (") (" + (Data_Show.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + (") (" + (Data_Show.show(dictShow)(v.value4) + (") (" + (Data_Show.show(dictShow1)(v.value5) + (") (" + (showTree(dictShow)(dictShow1)(v.value6) + ")")))))))))))));
            };
            throw new Error("Failed pattern match at Data.Map line 109, column 1 - line 110, column 1: " + [ v.constructor.name ]);
        };
    };
};
var showMap = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (m) {
            return "(fromFoldable " + (Data_Show.show(Data_Show.showArray(Data_Tuple.showTuple(dictShow)(dictShow1)))(toAscArray(m)) + ")");
        });
    };
};
var mapWithKey = function (v) {
    return function (v1) {
        if (v1 instanceof Leaf) {
            return Leaf.value;
        };
        if (v1 instanceof Two) {
            return new Two(mapWithKey(v)(v1.value0), v1.value1, v(v1.value1)(v1.value2), mapWithKey(v)(v1.value3));
        };
        if (v1 instanceof Three) {
            return new Three(mapWithKey(v)(v1.value0), v1.value1, v(v1.value1)(v1.value2), mapWithKey(v)(v1.value3), v1.value4, v(v1.value4)(v1.value5), mapWithKey(v)(v1.value6));
        };
        throw new Error("Failed pattern match at Data.Map line 451, column 1 - line 451, column 25: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var lookupLE = function (dictOrd) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v1 instanceof Two) {
                var v2 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v1.value1, 
                        value: v1.value2
                    });
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupLE(dictOrd)(v)(v1.value3)));
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return lookupLE(dictOrd)(v)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Map line 176, column 37 - line 179, column 24: " + [ v2.constructor.name ]);
            };
            if (v1 instanceof Three) {
                var v3 = Data_Ord.compare(dictOrd)(v)(v1.value4);
                if (v3 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v1.value4, 
                        value: v1.value5
                    });
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value4, 
                        value: v1.value5
                    })(lookupLE(dictOrd)(v)(v1.value6)));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return lookupLE(dictOrd)(v)(new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                };
                throw new Error("Failed pattern match at Data.Map line 180, column 49 - line 183, column 40: " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map line 175, column 1 - line 175, column 26: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var lookupGE = function (dictOrd) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v1 instanceof Two) {
                var v2 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v1.value1, 
                        value: v1.value2
                    });
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupGE(dictOrd)(v)(v1.value0)));
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return lookupGE(dictOrd)(v)(v1.value3);
                };
                throw new Error("Failed pattern match at Data.Map line 200, column 37 - line 203, column 25: " + [ v2.constructor.name ]);
            };
            if (v1 instanceof Three) {
                var v3 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v3 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v1.value1, 
                        value: v1.value2
                    });
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupGE(dictOrd)(v)(v1.value0)));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return lookupGE(dictOrd)(v)(new Two(v1.value3, v1.value4, v1.value5, v1.value6));
                };
                throw new Error("Failed pattern match at Data.Map line 204, column 49 - line 207, column 41: " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map line 199, column 1 - line 199, column 26: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var lookup = function (dictOrd) {
    return function (k) {
        return function (tree) {
            if (tree instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            var comp = Data_Ord.compare(dictOrd);
            var __unused = function (dictPartial1) {
                return function ($dollar45) {
                    return $dollar45;
                };
            };
            return __unused()((function () {
                if (tree instanceof Two) {
                    var v1 = comp(k)(tree.value1);
                    if (v1 instanceof Data_Ordering.EQ) {
                        return new Data_Maybe.Just(tree.value2);
                    };
                    if (v1 instanceof Data_Ordering.LT) {
                        return lookup(dictOrd)(k)(tree.value0);
                    };
                    return lookup(dictOrd)(k)(tree.value3);
                };
                if (tree instanceof Three) {
                    var v = comp(k)(tree.value1);
                    if (v instanceof Data_Ordering.EQ) {
                        return new Data_Maybe.Just(tree.value2);
                    };
                    var v3 = comp(k)(tree.value4);
                    if (v3 instanceof Data_Ordering.EQ) {
                        return new Data_Maybe.Just(tree.value5);
                    };
                    if (v instanceof Data_Ordering.LT) {
                        return lookup(dictOrd)(k)(tree.value0);
                    };
                    if (v3 instanceof Data_Ordering.GT) {
                        return lookup(dictOrd)(k)(tree.value6);
                    };
                    return lookup(dictOrd)(k)(tree.value3);
                };
                throw new Error("Failed pattern match at Data.Map line 156, column 10 - line 170, column 39: " + [ tree.constructor.name ]);
            })());
        };
    };
};
var member = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
        };
    };
};
var keys = function (v) {
    if (v instanceof Leaf) {
        return Data_List_Types.Nil.value;
    };
    if (v instanceof Two) {
        return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(keys(v.value3)));
    };
    if (v instanceof Three) {
        return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value4))(keys(v.value6)))));
    };
    throw new Error("Failed pattern match at Data.Map line 419, column 1 - line 419, column 16: " + [ v.constructor.name ]);
};
var isEmpty = function (v) {
    if (v instanceof Leaf) {
        return true;
    };
    return false;
};
var functorMap = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Leaf) {
            return Leaf.value;
        };
        if (v1 instanceof Two) {
            return new Two(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3));
        };
        if (v1 instanceof Three) {
            return new Three(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Data_Functor.map(functorMap)(v)(v1.value6));
        };
        throw new Error("Failed pattern match at Data.Map line 81, column 3 - line 81, column 20: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var fromZipper = function (__copy_dictOrd) {
    return function (__copy_v) {
        return function (__copy_tree) {
            var __tco_dictOrd = __copy_dictOrd;
            var __tco_v = __copy_v;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(dictOrd, v, tree) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return tree;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof TwoLeft) {
                        __tco_dictOrd = dictOrd;
                        __tco_v = v.value1;
                        __copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
                        return;
                    };
                    if (v.value0 instanceof TwoRight) {
                        __tco_dictOrd = dictOrd;
                        __tco_v = v.value1;
                        __copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
                        return;
                    };
                    if (v.value0 instanceof ThreeLeft) {
                        __tco_dictOrd = dictOrd;
                        __tco_v = v.value1;
                        __copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                        return;
                    };
                    if (v.value0 instanceof ThreeMiddle) {
                        __tco_dictOrd = dictOrd;
                        __tco_v = v.value1;
                        __copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
                        return;
                    };
                    if (v.value0 instanceof ThreeRight) {
                        __tco_dictOrd = dictOrd;
                        __tco_v = v.value1;
                        __copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map line 247, column 3 - line 252, column 88: " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 245, column 1 - line 245, column 27: " + [ v.constructor.name, tree.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_dictOrd, __tco_v, __copy_tree);
            };
            return __tco_result;
        };
    };
};
var insert = function (dictOrd) {
    var up = function (__copy_v) {
        return function (__copy_v1) {
            var __tco_v = __copy_v;
            var __tco_done = false;
            var __tco_result;
            function __tco_loop(v, v1) {
                if (v instanceof Data_List_Types.Nil) {
                    __tco_done = true;
                    return new Two(v1.value0, v1.value1, v1.value2, v1.value3);
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof TwoLeft) {
                        __tco_done = true;
                        return fromZipper(dictOrd)(v.value1)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, v.value0.value0, v.value0.value1, v.value0.value2));
                    };
                    if (v.value0 instanceof TwoRight) {
                        __tco_done = true;
                        return fromZipper(dictOrd)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0, v1.value1, v1.value2, v1.value3));
                    };
                    if (v.value0 instanceof ThreeLeft) {
                        __tco_v = v.value1;
                        __copy_v1 = new KickUp(new Two(v1.value0, v1.value1, v1.value2, v1.value3), v.value0.value0, v.value0.value1, new Two(v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5));
                        return;
                    };
                    if (v.value0 instanceof ThreeMiddle) {
                        __tco_v = v.value1;
                        __copy_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0), v1.value1, v1.value2, new Two(v1.value3, v.value0.value3, v.value0.value4, v.value0.value5));
                        return;
                    };
                    if (v.value0 instanceof ThreeRight) {
                        __tco_v = v.value1;
                        __copy_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3), v.value0.value4, v.value0.value5, new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map line 283, column 5 - line 288, column 104: " + [ v.value0.constructor.name, v1.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map line 281, column 3 - line 281, column 54: " + [ v.constructor.name, v1.constructor.name ]);
            };
            while (!__tco_done) {
                __tco_result = __tco_loop(__tco_v, __copy_v1);
            };
            return __tco_result;
        };
    };
    var comp = Data_Ord.compare(dictOrd);
    var down = function (__copy_ctx) {
        return function (__copy_k) {
            return function (__copy_v) {
                return function (__copy_v1) {
                    var __tco_ctx = __copy_ctx;
                    var __tco_k = __copy_k;
                    var __tco_v = __copy_v;
                    var __tco_done = false;
                    var __tco_result;
                    function __tco_loop(ctx, k, v, v1) {
                        if (v1 instanceof Leaf) {
                            __tco_done = true;
                            return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
                        };
                        if (v1 instanceof Two) {
                            var v3 = comp(k)(v1.value1);
                            if (v3 instanceof Data_Ordering.EQ) {
                                __tco_done = true;
                                return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                            };
                            if (v3 instanceof Data_Ordering.LT) {
                                __tco_ctx = new Data_List_Types.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                                __tco_k = k;
                                __tco_v = v;
                                __copy_v1 = v1.value0;
                                return;
                            };
                            __tco_ctx = new Data_List_Types.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                            __tco_k = k;
                            __tco_v = v;
                            __copy_v1 = v1.value3;
                            return;
                        };
                        if (v1 instanceof Three) {
                            var v3 = comp(k)(v1.value1);
                            if (v3 instanceof Data_Ordering.EQ) {
                                __tco_done = true;
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                            };
                            var v4 = comp(k)(v1.value4);
                            if (v4 instanceof Data_Ordering.EQ) {
                                __tco_done = true;
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                            };
                            if (v3 instanceof Data_Ordering.LT) {
                                __tco_ctx = new Data_List_Types.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                                __tco_k = k;
                                __tco_v = v;
                                __copy_v1 = v1.value0;
                                return;
                            };
                            if (v3 instanceof Data_Ordering.GT && v4 instanceof Data_Ordering.LT) {
                                __tco_ctx = new Data_List_Types.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                                __tco_k = k;
                                __tco_v = v;
                                __copy_v1 = v1.value3;
                                return;
                            };
                            __tco_ctx = new Data_List_Types.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                            __tco_k = k;
                            __tco_v = v;
                            __copy_v1 = v1.value6;
                            return;
                        };
                        throw new Error("Failed pattern match at Data.Map line 264, column 3 - line 264, column 52: " + [ ctx.constructor.name, k.constructor.name, v.constructor.name, v1.constructor.name ]);
                    };
                    while (!__tco_done) {
                        __tco_result = __tco_loop(__tco_ctx, __tco_k, __tco_v, __copy_v1);
                    };
                    return __tco_result;
                };
            };
        };
    };
    return down(Data_List_Types.Nil.value);
};
var pop = function (dictOrd) {
    var up = function (ctxs) {
        return function (tree) {
            if (ctxs instanceof Data_List_Types.Nil) {
                return tree;
            };
            if (ctxs instanceof Data_List_Types.Cons) {
                var __unused = function (dictPartial1) {
                    return function ($dollar53) {
                        return $dollar53;
                    };
                };
                return __unused()((function () {
                    if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
                    };
                    if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
                    };
                    if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                        return up(ctxs.value1)(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3));
                    };
                    if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                        return up(ctxs.value1)(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree));
                    };
                    if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
                    };
                    if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
                    };
                    if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                    };
                    if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                    };
                    if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
                    };
                    if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                    };
                    if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                    };
                    if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
                    };
                    if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
                    };
                    if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                    };
                    if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                    };
                    if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
                    };
                    if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                        return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
                    };
                    throw new Error("Failed pattern match at Data.Map line 333, column 9 - line 350, column 136: " + [ ctxs.value0.constructor.name, tree.constructor.name ]);
                })());
            };
            throw new Error("Failed pattern match at Data.Map line 330, column 5 - line 350, column 136: " + [ ctxs.constructor.name ]);
        };
    };
    var removeMaxNode = function (ctx) {
        return function (m) {
            var __unused = function (dictPartial1) {
                return function ($dollar55) {
                    return $dollar55;
                };
            };
            return __unused()((function () {
                if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
                    return up(ctx)(Leaf.value);
                };
                if (m instanceof Two) {
                    return removeMaxNode(new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx))(m.value3);
                };
                if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
                    return up(new Data_List_Types.Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
                };
                if (m instanceof Three) {
                    return removeMaxNode(new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx))(m.value6);
                };
                throw new Error("Failed pattern match at Data.Map line 362, column 5 - line 366, column 107: " + [ m.constructor.name ]);
            })());
        };
    };
    var maxNode = function (m) {
        var __unused = function (dictPartial1) {
            return function ($dollar57) {
                return $dollar57;
            };
        };
        return __unused()((function () {
            if (m instanceof Two && m.value3 instanceof Leaf) {
                return {
                    key: m.value1, 
                    value: m.value2
                };
            };
            if (m instanceof Two) {
                return maxNode(m.value3);
            };
            if (m instanceof Three && m.value6 instanceof Leaf) {
                return {
                    key: m.value4, 
                    value: m.value5
                };
            };
            if (m instanceof Three) {
                return maxNode(m.value6);
            };
            throw new Error("Failed pattern match at Data.Map line 353, column 33 - line 357, column 45: " + [ m.constructor.name ]);
        })());
    };
    var comp = Data_Ord.compare(dictOrd);
    var down = function (__copy_ctx) {
        return function (__copy_k) {
            return function (__copy_m) {
                var __tco_ctx = __copy_ctx;
                var __tco_k = __copy_k;
                var __tco_done = false;
                var __tco_result;
                function __tco_loop(ctx, k, m) {
                    if (m instanceof Leaf) {
                        __tco_done = true;
                        return Data_Maybe.Nothing.value;
                    };
                    if (m instanceof Two) {
                        var v = comp(k)(m.value1);
                        if (m.value3 instanceof Leaf && v instanceof Data_Ordering.EQ) {
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, up(ctx)(Leaf.value)));
                        };
                        if (v instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value0);
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new TwoLeft(max.key, max.value, m.value3), ctx))(m.value0)));
                        };
                        if (v instanceof Data_Ordering.LT) {
                            __tco_ctx = new Data_List_Types.Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                            __tco_k = k;
                            __copy_m = m.value0;
                            return;
                        };
                        __tco_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                        __tco_k = k;
                        __copy_m = m.value3;
                        return;
                    };
                    if (m instanceof Three) {
                        var leaves = (function () {
                            if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                                return true;
                            };
                            return false;
                        })();
                        var v = comp(k)(m.value4);
                        var v3 = comp(k)(m.value1);
                        if (leaves && v3 instanceof Data_Ordering.EQ) {
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
                        };
                        if (leaves && v instanceof Data_Ordering.EQ) {
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
                        };
                        if (v3 instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value0);
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new ThreeLeft(max.key, max.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
                        };
                        if (v instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value3);
                            __tco_done = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, removeMaxNode(new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max.key, max.value, m.value6), ctx))(m.value3)));
                        };
                        if (v3 instanceof Data_Ordering.LT) {
                            __tco_ctx = new Data_List_Types.Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                            __tco_k = k;
                            __copy_m = m.value0;
                            return;
                        };
                        if (v3 instanceof Data_Ordering.GT && v instanceof Data_Ordering.LT) {
                            __tco_ctx = new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                            __tco_k = k;
                            __copy_m = m.value3;
                            return;
                        };
                        __tco_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                        __tco_k = k;
                        __copy_m = m.value6;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map line 303, column 36 - line 326, column 82: " + [ m.constructor.name ]);
                };
                while (!__tco_done) {
                    __tco_result = __tco_loop(__tco_ctx, __tco_k, __copy_m);
                };
                return __tco_result;
            };
        };
    };
    return down(Data_List_Types.Nil.value);
};
var foldableMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (m) {
            return Data_Foldable.foldMap(Data_List_Types.foldableList)(dictMonoid)(f)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldl(Data_List_Types.foldableList)(f)(z)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_List_Types.foldableList)(f)(z)(values(m));
        };
    };
});
var traversableMap = new Data_Traversable.Traversable(function () {
    return foldableMap;
}, function () {
    return functorMap;
}, function (dictApplicative) {
    return Data_Traversable.traverse(traversableMap)(dictApplicative)(Control_Category.id(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            if (v instanceof Leaf) {
                return Control_Applicative.pure(dictApplicative)(Leaf.value);
            };
            if (v instanceof Two) {
                return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Two.create)(Data_Traversable.traverse(traversableMap)(dictApplicative)(f)(v.value0)))(Control_Applicative.pure(dictApplicative)(v.value1)))(f(v.value2)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(f)(v.value3));
            };
            if (v instanceof Three) {
                return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Three.create)(Data_Traversable.traverse(traversableMap)(dictApplicative)(f)(v.value0)))(Control_Applicative.pure(dictApplicative)(v.value1)))(f(v.value2)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(f)(v.value3)))(Control_Applicative.pure(dictApplicative)(v.value4)))(f(v.value5)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(f)(v.value6));
            };
            throw new Error("Failed pattern match at Data.Map line 91, column 3 - line 91, column 30: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
});
var findMin = function (v) {
    if (v instanceof Leaf) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Two) {
        return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
            key: v.value1, 
            value: v.value2
        })(findMin(v.value0)));
    };
    if (v instanceof Three) {
        return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
            key: v.value1, 
            value: v.value2
        })(findMin(v.value0)));
    };
    throw new Error("Failed pattern match at Data.Map line 229, column 1 - line 229, column 23: " + [ v.constructor.name ]);
};
var lookupGT = function (dictOrd) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v1 instanceof Two) {
                var v2 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return findMin(v1.value3);
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupGT(dictOrd)(v)(v1.value0)));
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return lookupGT(dictOrd)(v)(v1.value3);
                };
                throw new Error("Failed pattern match at Data.Map line 212, column 37 - line 215, column 25: " + [ v2.constructor.name ]);
            };
            if (v1 instanceof Three) {
                var v3 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v3 instanceof Data_Ordering.EQ) {
                    return findMin(new Two(v1.value3, v1.value4, v1.value5, v1.value6));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupGT(dictOrd)(v)(v1.value0)));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return lookupGT(dictOrd)(v)(new Two(v1.value3, v1.value4, v1.value5, v1.value6));
                };
                throw new Error("Failed pattern match at Data.Map line 216, column 49 - line 219, column 41: " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map line 211, column 1 - line 211, column 26: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var findMax = function (v) {
    if (v instanceof Leaf) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Two) {
        return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
            key: v.value1, 
            value: v.value2
        })(findMax(v.value3)));
    };
    if (v instanceof Three) {
        return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
            key: v.value4, 
            value: v.value5
        })(findMax(v.value6)));
    };
    throw new Error("Failed pattern match at Data.Map line 223, column 1 - line 223, column 23: " + [ v.constructor.name ]);
};
var lookupLT = function (dictOrd) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v1 instanceof Two) {
                var v2 = Data_Ord.compare(dictOrd)(v)(v1.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return findMax(v1.value0);
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value1, 
                        value: v1.value2
                    })(lookupLT(dictOrd)(v)(v1.value3)));
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return lookupLT(dictOrd)(v)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Map line 188, column 37 - line 191, column 24: " + [ v2.constructor.name ]);
            };
            if (v1 instanceof Three) {
                var v3 = Data_Ord.compare(dictOrd)(v)(v1.value4);
                if (v3 instanceof Data_Ordering.EQ) {
                    return findMax(new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return Data_Maybe.Just.create(Data_Maybe.fromMaybe({
                        key: v1.value4, 
                        value: v1.value5
                    })(lookupLT(dictOrd)(v)(v1.value6)));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return lookupLT(dictOrd)(v)(new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                };
                throw new Error("Failed pattern match at Data.Map line 192, column 49 - line 195, column 40: " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map line 187, column 1 - line 187, column 26: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var eqMap = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (m1) {
            return function (m2) {
                return Data_Eq.eq(Data_Eq.eqArray(Data_Tuple.eqTuple(dictEq)(dictEq1)))(toAscArray(m1))(toAscArray(m2));
            };
        });
    };
};
var ordMap = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqMap(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (m1) {
            return function (m2) {
                return Data_Ord.compare(Data_Ord.ordArray(Data_Tuple.ordTuple(dictOrd)(dictOrd1)))(toAscArray(m1))(toAscArray(m2));
            };
        });
    };
};
var eq1Map = function (dictEq) {
    return new Data_Eq.Eq1(function (dictEq1) {
        return Data_Eq.eq(eqMap(dictEq)(dictEq1));
    });
};
var ord1Map = function (dictOrd) {
    return new Data_Ord.Ord1(function () {
        return eq1Map(dictOrd.Eq0());
    }, function (dictOrd1) {
        return Data_Ord.compare(ordMap(dictOrd)(dictOrd1));
    });
};
var empty = Leaf.value;
var fromFoldable = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(function (m) {
            return function (v) {
                return insert(dictOrd)(v.value0)(v.value1)(m);
            };
        })(empty);
    };
};
var $$delete = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.maybe(m)(Data_Tuple.snd)(pop(dictOrd)(k)(m));
        };
    };
};
var checkValid = function (tree) {
    var allHeights = function (v) {
        if (v instanceof Leaf) {
            return Control_Applicative.pure(Data_List_Types.applicativeList)(0);
        };
        if (v instanceof Two) {
            return Data_Functor.map(Data_List_Types.functorList)(function (n) {
                return n + 1 | 0;
            })(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value0))(allHeights(v.value3)));
        };
        if (v instanceof Three) {
            return Data_Functor.map(Data_List_Types.functorList)(function (n) {
                return n + 1 | 0;
            })(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value3))(allHeights(v.value6))));
        };
        throw new Error("Failed pattern match at Data.Map line 144, column 3 - line 144, column 30: " + [ v.constructor.name ]);
    };
    return Data_List.length(Data_List.nub(Data_Eq.eqInt)(allHeights(tree))) === 1;
};
var alter = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                var v = f(lookup(dictOrd)(k)(m));
                if (v instanceof Data_Maybe.Nothing) {
                    return $$delete(dictOrd)(k)(m);
                };
                if (v instanceof Data_Maybe.Just) {
                    return insert(dictOrd)(k)(v.value0)(m);
                };
                throw new Error("Failed pattern match at Data.Map line 371, column 15 - line 373, column 25: " + [ v.constructor.name ]);
            };
        };
    };
};
var fromFoldableWith = function (dictOrd) {
    return function (dictFoldable) {
        return function (f) {
            var combine = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_Maybe.Just) {
                        return Data_Maybe.Just.create(f(v)(v1.value0));
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return new Data_Maybe.Just(v);
                    };
                    throw new Error("Failed pattern match at Data.Map line 388, column 3 - line 388, column 38: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Foldable.foldl(dictFoldable)(function (m) {
                return function (v) {
                    return alter(dictOrd)(combine(v.value1))(v.value0)(m);
                };
            })(empty);
        };
    };
};
var unionWith = function (dictOrd) {
    return function (f) {
        return function (m1) {
            return function (m2) {
                var go = function (m) {
                    return function (v) {
                        return alter(dictOrd)(function ($674) {
                            return Data_Maybe.Just.create(Data_Maybe.maybe(v.value1)(f(v.value1))($674));
                        })(v.value0)(m);
                    };
                };
                return Data_Foldable.foldl(Data_List_Types.foldableList)(go)(m2)(toUnfoldable(Data_List_Types.unfoldableList)(m1));
            };
        };
    };
};
var union = function (dictOrd) {
    return unionWith(dictOrd)(Data_Function["const"]);
};
var semigroupMap = function (dictOrd) {
    return new Data_Semigroup.Semigroup(union(dictOrd));
};
var monoidMap = function (dictOrd) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMap(dictOrd);
    }, empty);
};
var unions = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(union(dictOrd))(empty);
    };
};
var update = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                return alter(dictOrd)(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
            };
        };
    };
};
module.exports = {
    alter: alter, 
    checkValid: checkValid, 
    "delete": $$delete, 
    empty: empty, 
    findMax: findMax, 
    findMin: findMin, 
    fromFoldable: fromFoldable, 
    fromFoldableWith: fromFoldableWith, 
    insert: insert, 
    isEmpty: isEmpty, 
    keys: keys, 
    lookup: lookup, 
    lookupGE: lookupGE, 
    lookupGT: lookupGT, 
    lookupLE: lookupLE, 
    lookupLT: lookupLT, 
    mapWithKey: mapWithKey, 
    member: member, 
    pop: pop, 
    showTree: showTree, 
    singleton: singleton, 
    size: size, 
    toAscUnfoldable: toAscUnfoldable, 
    toUnfoldable: toUnfoldable, 
    union: union, 
    unionWith: unionWith, 
    unions: unions, 
    update: update, 
    values: values, 
    eq1Map: eq1Map, 
    eqMap: eqMap, 
    ord1Map: ord1Map, 
    ordMap: ordMap, 
    showMap: showMap, 
    semigroupMap: semigroupMap, 
    monoidMap: monoidMap, 
    functorMap: functorMap, 
    foldableMap: foldableMap, 
    traversableMap: traversableMap
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unfoldable":147,"../Partial.Unsafe":168,"../Prelude":171}],107:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "First (" + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupFirst = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        return v1;
    };
});
var ordFirst = function (dictOrd) {
    return Data_Maybe.ordMaybe(dictOrd);
};
var ord1First = Data_Maybe.ord1Maybe;
var newtypeFirst = new Data_Newtype.Newtype(function (n) {
    return n;
}, First);
var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
}, Data_Maybe.Nothing.value);
var monadFirst = Data_Maybe.monadMaybe;
var invariantFirst = Data_Maybe.invariantMaybe;
var functorFirst = Data_Maybe.functorMaybe;
var extendFirst = Data_Maybe.extendMaybe;
var eqFirst = function (dictEq) {
    return Data_Maybe.eqMaybe(dictEq);
};
var eq1First = Data_Maybe.eq1Maybe;
var boundedFirst = function (dictBounded) {
    return Data_Maybe.boundedMaybe(dictBounded);
};
var bindFirst = Data_Maybe.bindMaybe;
var applyFirst = Data_Maybe.applyMaybe;
var applicativeFirst = Data_Maybe.applicativeMaybe;
module.exports = {
    First: First, 
    newtypeFirst: newtypeFirst, 
    eqFirst: eqFirst, 
    eq1First: eq1First, 
    ordFirst: ordFirst, 
    ord1First: ord1First, 
    boundedFirst: boundedFirst, 
    functorFirst: functorFirst, 
    invariantFirst: invariantFirst, 
    applyFirst: applyFirst, 
    applicativeFirst: applicativeFirst, 
    bindFirst: bindFirst, 
    monadFirst: monadFirst, 
    extendFirst: extendFirst, 
    showFirst: showFirst, 
    semigroupFirst: semigroupFirst, 
    monoidFirst: monoidFirst
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],108:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Last " + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupLast = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Just) {
            return v1;
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            return v;
        };
        throw new Error("Failed pattern match at Data.Maybe.Last line 54, column 3 - line 54, column 39: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var ordLast = function (dictOrd) {
    return Data_Maybe.ordMaybe(dictOrd);
};
var ord1Last = Data_Maybe.ord1Maybe;
var newtypeLast = new Data_Newtype.Newtype(function (n) {
    return n;
}, Last);
var monoidLast = new Data_Monoid.Monoid(function () {
    return semigroupLast;
}, Data_Maybe.Nothing.value);
var monadLast = Data_Maybe.monadMaybe;
var invariantLast = Data_Maybe.invariantMaybe;
var functorLast = Data_Maybe.functorMaybe;
var extendLast = Data_Maybe.extendMaybe;
var eqLast = function (dictEq) {
    return Data_Maybe.eqMaybe(dictEq);
};
var eq1Last = Data_Maybe.eq1Maybe;
var boundedLast = function (dictBounded) {
    return Data_Maybe.boundedMaybe(dictBounded);
};
var bindLast = Data_Maybe.bindMaybe;
var applyLast = Data_Maybe.applyMaybe;
var applicativeLast = Data_Maybe.applicativeMaybe;
module.exports = {
    Last: Last, 
    newtypeLast: newtypeLast, 
    eqLast: eqLast, 
    eq1Last: eq1Last, 
    ordLast: ordLast, 
    ord1Last: ord1Last, 
    boundedLast: boundedLast, 
    functorLast: functorLast, 
    invariantLast: invariantLast, 
    applyLast: applyLast, 
    applicativeLast: applicativeLast, 
    bindLast: bindLast, 
    monadLast: monadLast, 
    extendLast: extendLast, 
    showLast: showLast, 
    semigroupLast: semigroupLast, 
    monoidLast: monoidLast
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],109:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Control_MonadZero = require("../Control.MonadZero");
var Control_Plus = require("../Control.Plus");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var showMaybe = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Just) {
            return "(Just " + (Data_Show.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match at Data.Maybe line 208, column 3 - line 209, column 3: " + [ v.constructor.name ]);
    });
};
var semigroupMaybe = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            if (v1 instanceof Nothing) {
                return v;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return new Just(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Maybe line 177, column 3 - line 177, column 23: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var monoidMaybe = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
};
var maybe$prime = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v(Data_Unit.unit);
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 233, column 1 - line 233, column 28: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var maybe = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v;
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 220, column 1 - line 220, column 22: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Data_Function["const"](false));
var isJust = maybe(false)(Data_Function["const"](true));
var functorMaybe = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Just) {
            return new Just(v(v1.value0));
        };
        return Nothing.value;
    };
});
var invariantMaybe = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorMaybe));
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Control_Category.id(Control_Category.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Control_Category.id(Control_Category.categoryFn));
};
var fromJust = function (dictPartial) {
    return function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar34) {
                return $dollar34;
            };
        };
        return __unused(dictPartial)((function () {
            if (v instanceof Just) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Data.Maybe line 271, column 1 - line 271, column 21: " + [ v.constructor.name ]);
        })());
    };
};
var extendMaybe = new Control_Extend.Extend(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(v(v1));
    };
});
var eqMaybe = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            if (x instanceof Nothing && y instanceof Nothing) {
                return true;
            };
            if (x instanceof Just && y instanceof Just) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0);
            };
            return false;
        };
    });
};
var ordMaybe = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMaybe(dictOrd.Eq0());
    }, function (x) {
        return function (y) {
            if (x instanceof Nothing && y instanceof Nothing) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Nothing) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Nothing) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Just && y instanceof Just) {
                return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 196, column 1 - line 196, column 51: " + [ x.constructor.name, y.constructor.name ]);
        };
    });
};
var eq1Maybe = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqMaybe(dictEq));
});
var ord1Maybe = new Data_Ord.Ord1(function () {
    return eq1Maybe;
}, function (dictOrd) {
    return Data_Ord.compare(ordMaybe(dictOrd));
});
var boundedMaybe = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordMaybe(dictBounded.Ord0());
    }, Nothing.value, new Just(Data_Bounded.top(dictBounded)));
};
var applyMaybe = new Control_Apply.Apply(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return Data_Functor.map(functorMaybe)(v.value0)(v1);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 69, column 3 - line 69, column 31: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindMaybe = new Control_Bind.Bind(function () {
    return applyMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return v1(v.value0);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 128, column 3 - line 128, column 24: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var applicativeMaybe = new Control_Applicative.Applicative(function () {
    return applyMaybe;
}, Just.create);
var monadMaybe = new Control_Monad.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});
var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Nothing) {
            return v1;
        };
        return v;
    };
});
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return applicativeMaybe;
}, function () {
    return plusMaybe;
});
var monadZeroMaybe = new Control_MonadZero.MonadZero(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Nothing: Nothing, 
    Just: Just, 
    fromJust: fromJust, 
    fromMaybe: fromMaybe, 
    "fromMaybe'": fromMaybe$prime, 
    isJust: isJust, 
    isNothing: isNothing, 
    maybe: maybe, 
    "maybe'": maybe$prime, 
    functorMaybe: functorMaybe, 
    applyMaybe: applyMaybe, 
    applicativeMaybe: applicativeMaybe, 
    altMaybe: altMaybe, 
    plusMaybe: plusMaybe, 
    alternativeMaybe: alternativeMaybe, 
    bindMaybe: bindMaybe, 
    monadMaybe: monadMaybe, 
    monadZeroMaybe: monadZeroMaybe, 
    extendMaybe: extendMaybe, 
    invariantMaybe: invariantMaybe, 
    semigroupMaybe: semigroupMaybe, 
    monoidMaybe: monoidMaybe, 
    eqMaybe: eqMaybe, 
    eq1Maybe: eq1Maybe, 
    ordMaybe: ordMaybe, 
    ord1Maybe: ord1Maybe, 
    boundedMaybe: boundedMaybe, 
    showMaybe: showMaybe
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Extend":15,"../Control.Monad":38,"../Control.MonadZero":40,"../Control.Plus":41,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Function":88,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Unit":149,"../Prelude":171}],110:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Additive " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupAdditive = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.add(dictSemiring)(v)(v1);
        };
    });
};
var ordAdditive = function (dictOrd) {
    return dictOrd;
};
var newtypeAdditive = new Data_Newtype.Newtype(function (n) {
    return n;
}, Additive);
var monoidAdditive = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAdditive(dictSemiring);
    }, Data_Semiring.zero(dictSemiring));
};
var invariantAdditive = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorAdditive = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendAdditive = new Control_Extend.Extend(function () {
    return functorAdditive;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqAdditive = function (dictEq) {
    return dictEq;
};
var comonadAdditive = new Control_Comonad.Comonad(function () {
    return extendAdditive;
}, Data_Newtype.unwrap(newtypeAdditive));
var boundedAdditive = function (dictBounded) {
    return dictBounded;
};
var applyAdditive = new Control_Apply.Apply(function () {
    return functorAdditive;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindAdditive = new Control_Bind.Bind(function () {
    return applyAdditive;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeAdditive = new Control_Applicative.Applicative(function () {
    return applyAdditive;
}, Additive);
var monadAdditive = new Control_Monad.Monad(function () {
    return applicativeAdditive;
}, function () {
    return bindAdditive;
});
module.exports = {
    Additive: Additive, 
    newtypeAdditive: newtypeAdditive, 
    eqAdditive: eqAdditive, 
    ordAdditive: ordAdditive, 
    boundedAdditive: boundedAdditive, 
    functorAdditive: functorAdditive, 
    invariantAdditive: invariantAdditive, 
    applyAdditive: applyAdditive, 
    applicativeAdditive: applicativeAdditive, 
    bindAdditive: bindAdditive, 
    monadAdditive: monadAdditive, 
    extendAdditive: extendAdditive, 
    comonadAdditive: comonadAdditive, 
    showAdditive: showAdditive, 
    semigroupAdditive: semigroupAdditive, 
    monoidAdditive: monoidAdditive
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Prelude":171}],111:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Conj = function (x) {
    return x;
};
var showConj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Conj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringConj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var semigroupConj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var ordConj = function (dictOrd) {
    return dictOrd;
};
var newtypeConj = new Data_Newtype.Newtype(function (n) {
    return n;
}, Conj);
var monoidConj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var invariantConj = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorConj = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendConj = new Control_Extend.Extend(function () {
    return functorConj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqConj = function (dictEq) {
    return dictEq;
};
var comonadConj = new Control_Comonad.Comonad(function () {
    return extendConj;
}, Data_Newtype.unwrap(newtypeConj));
var boundedConj = function (dictBounded) {
    return dictBounded;
};
var applyConj = new Control_Apply.Apply(function () {
    return functorConj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindConj = new Control_Bind.Bind(function () {
    return applyConj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeConj = new Control_Applicative.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Control_Monad.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj, 
    newtypeConj: newtypeConj, 
    eqConj: eqConj, 
    ordConj: ordConj, 
    boundedConj: boundedConj, 
    functorConj: functorConj, 
    invariantConj: invariantConj, 
    applyConj: applyConj, 
    applicativeConj: applicativeConj, 
    bindConj: bindConj, 
    monadConj: monadConj, 
    extendConj: extendConj, 
    comonadConj: comonadConj, 
    showConj: showConj, 
    semigroupConj: semigroupConj, 
    monoidConj: monoidConj, 
    semiringConj: semiringConj
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.HeytingAlgebra":96,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Prelude":171}],112:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Disj = function (x) {
    return x;
};
var showDisj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Disj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringDisj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var semigroupDisj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var ordDisj = function (dictOrd) {
    return dictOrd;
};
var newtypeDisj = new Data_Newtype.Newtype(function (n) {
    return n;
}, Disj);
var monoidDisj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDisj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var invariantDisj = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorDisj = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDisj = new Control_Extend.Extend(function () {
    return functorDisj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDisj = function (dictEq) {
    return dictEq;
};
var comonadDisj = new Control_Comonad.Comonad(function () {
    return extendDisj;
}, Data_Newtype.unwrap(newtypeDisj));
var boundedDisj = function (dictBounded) {
    return dictBounded;
};
var applyDisj = new Control_Apply.Apply(function () {
    return functorDisj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDisj = new Control_Bind.Bind(function () {
    return applyDisj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDisj = new Control_Applicative.Applicative(function () {
    return applyDisj;
}, Disj);
var monadDisj = new Control_Monad.Monad(function () {
    return applicativeDisj;
}, function () {
    return bindDisj;
});
module.exports = {
    Disj: Disj, 
    newtypeDisj: newtypeDisj, 
    eqDisj: eqDisj, 
    ordDisj: ordDisj, 
    boundedDisj: boundedDisj, 
    functorDisj: functorDisj, 
    invariantDisj: invariantDisj, 
    applyDisj: applyDisj, 
    applicativeDisj: applicativeDisj, 
    bindDisj: bindDisj, 
    monadDisj: monadDisj, 
    extendDisj: extendDisj, 
    comonadDisj: comonadDisj, 
    showDisj: showDisj, 
    semigroupDisj: semigroupDisj, 
    monoidDisj: monoidDisj, 
    semiringDisj: semiringDisj
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.HeytingAlgebra":96,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Prelude":171}],113:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Dual = function (x) {
    return x;
};
var showDual = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Dual " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupDual = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v1)(v);
        };
    });
};
var ordDual = function (dictOrd) {
    return dictOrd;
};
var newtypeDual = new Data_Newtype.Newtype(function (n) {
    return n;
}, Dual);
var monoidDual = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDual(dictMonoid.Semigroup0());
    }, Data_Monoid.mempty(dictMonoid));
};
var invariantDual = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorDual = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDual = new Control_Extend.Extend(function () {
    return functorDual;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDual = function (dictEq) {
    return dictEq;
};
var comonadDual = new Control_Comonad.Comonad(function () {
    return extendDual;
}, Data_Newtype.unwrap(newtypeDual));
var boundedDual = function (dictBounded) {
    return dictBounded;
};
var applyDual = new Control_Apply.Apply(function () {
    return functorDual;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDual = new Control_Bind.Bind(function () {
    return applyDual;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDual = new Control_Applicative.Applicative(function () {
    return applyDual;
}, Dual);
var monadDual = new Control_Monad.Monad(function () {
    return applicativeDual;
}, function () {
    return bindDual;
});
module.exports = {
    Dual: Dual, 
    newtypeDual: newtypeDual, 
    eqDual: eqDual, 
    ordDual: ordDual, 
    boundedDual: boundedDual, 
    functorDual: functorDual, 
    invariantDual: invariantDual, 
    applyDual: applyDual, 
    applicativeDual: applicativeDual, 
    bindDual: bindDual, 
    monadDual: monadDual, 
    extendDual: extendDual, 
    comonadDual: comonadDual, 
    showDual: showDual, 
    semigroupDual: semigroupDual, 
    monoidDual: monoidDual
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],114:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Prelude = require("../Prelude");
var Endo = function (x) {
    return x;
};
var semigroupEndo = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return function ($11) {
            return v(v1($11));
        };
    };
});
var newtypeEndo = new Data_Newtype.Newtype(function (n) {
    return n;
}, Endo);
var monoidEndo = new Data_Monoid.Monoid(function () {
    return semigroupEndo;
}, Control_Category.id(Control_Category.categoryFn));
var invariantEndo = new Data_Functor_Invariant.Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($12) {
                return ab(v(ba($12)));
            };
        };
    };
});
module.exports = {
    Endo: Endo, 
    newtypeEndo: newtypeEndo, 
    invariantEndo: invariantEndo, 
    semigroupEndo: semigroupEndo, 
    monoidEndo: monoidEndo
};

},{"../Control.Category":13,"../Control.Semigroupoid":42,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Semigroup":128,"../Prelude":171}],115:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Monad = require("../Control.Monad");
var Data_Bounded = require("../Data.Bounded");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Multiplicative = function (x) {
    return x;
};
var showMultiplicative = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Multiplicative " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupMultiplicative = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.mul(dictSemiring)(v)(v1);
        };
    });
};
var ordMultiplicative = function (dictOrd) {
    return dictOrd;
};
var newtypeMultiplicative = new Data_Newtype.Newtype(function (n) {
    return n;
}, Multiplicative);
var monoidMultiplicative = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMultiplicative(dictSemiring);
    }, Data_Semiring.one(dictSemiring));
};
var invariantMultiplicative = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorMultiplicative = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendMultiplicative = new Control_Extend.Extend(function () {
    return functorMultiplicative;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqMultiplicative = function (dictEq) {
    return dictEq;
};
var comonadMultiplicative = new Control_Comonad.Comonad(function () {
    return extendMultiplicative;
}, Data_Newtype.unwrap(newtypeMultiplicative));
var boundedMultiplicative = function (dictBounded) {
    return dictBounded;
};
var applyMultiplicative = new Control_Apply.Apply(function () {
    return functorMultiplicative;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindMultiplicative = new Control_Bind.Bind(function () {
    return applyMultiplicative;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeMultiplicative = new Control_Applicative.Applicative(function () {
    return applyMultiplicative;
}, Multiplicative);
var monadMultiplicative = new Control_Monad.Monad(function () {
    return applicativeMultiplicative;
}, function () {
    return bindMultiplicative;
});
module.exports = {
    Multiplicative: Multiplicative, 
    newtypeMultiplicative: newtypeMultiplicative, 
    eqMultiplicative: eqMultiplicative, 
    ordMultiplicative: ordMultiplicative, 
    boundedMultiplicative: boundedMultiplicative, 
    functorMultiplicative: functorMultiplicative, 
    invariantMultiplicative: invariantMultiplicative, 
    applyMultiplicative: applyMultiplicative, 
    applicativeMultiplicative: applicativeMultiplicative, 
    bindMultiplicative: bindMultiplicative, 
    monadMultiplicative: monadMultiplicative, 
    extendMultiplicative: extendMultiplicative, 
    comonadMultiplicative: comonadMultiplicative, 
    showMultiplicative: showMultiplicative, 
    semigroupMultiplicative: semigroupMultiplicative, 
    monoidMultiplicative: monoidMultiplicative
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Monad":38,"../Data.Bounded":66,"../Data.Eq":78,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Prelude":171}],116:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Monoid = function (Semigroup0, mempty) {
    this.Semigroup0 = Semigroup0;
    this.mempty = mempty;
};
var monoidUnit = new Monoid(function () {
    return Data_Semigroup.semigroupUnit;
}, Data_Unit.unit);
var monoidString = new Monoid(function () {
    return Data_Semigroup.semigroupString;
}, "");
var monoidArray = new Monoid(function () {
    return Data_Semigroup.semigroupArray;
}, [  ]);
var mempty = function (dict) {
    return dict.mempty;
};
var monoidFn = function (dictMonoid) {
    return new Monoid(function () {
        return Data_Semigroup.semigroupFn(dictMonoid.Semigroup0());
    }, Data_Function["const"](mempty(dictMonoid)));
};
var power = function (dictMonoid) {
    return function (x) {
        var go = function (p) {
            if (p <= 0) {
                return mempty(dictMonoid);
            };
            if (p === 1) {
                return x;
            };
            if (p % 2 === 0) {
                var x$prime = go(p / 2 | 0);
                return Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(x$prime);
            };
            if (Data_Boolean.otherwise) {
                var x$prime = go(p / 2 | 0);
                return Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(x));
            };
            throw new Error("Failed pattern match at Data.Monoid line 49, column 3 - line 53, column 57: " + [ p.constructor.name ]);
        };
        return go;
    };
};
module.exports = {
    Monoid: Monoid, 
    mempty: mempty, 
    power: power, 
    monoidUnit: monoidUnit, 
    monoidFn: monoidFn, 
    monoidString: monoidString, 
    monoidArray: monoidArray
};

},{"../Data.Boolean":63,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Function":88,"../Data.Ord":123,"../Data.Semigroup":128,"../Data.Unit":149,"../Prelude":171}],117:[function(require,module,exports){
arguments[4][54][0].apply(exports,arguments)
},{"dup":54}],118:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Prelude = require("../Prelude");
var Newtype = function (unwrap, wrap) {
    this.unwrap = unwrap;
    this.wrap = wrap;
};
var wrap = function (dict) {
    return dict.wrap;
};
var unwrap = function (dict) {
    return dict.unwrap;
};
var underF2 = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($50) {
                            return function ($51) {
                                return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(Data_Function.on(f)(Data_Functor.map(dictFunctor)(wrap(dictNewtype)))($50)($51));
                            };
                        };
                    };
                };
            };
        };
    };
};
var underF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($52) {
                            return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(wrap(dictNewtype))($52)));
                        };
                    };
                };
            };
        };
    };
};
var under2 = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($53) {
                    return function ($54) {
                        return unwrap(dictNewtype1)(Data_Function.on(f)(wrap(dictNewtype))($53)($54));
                    };
                };
            };
        };
    };
};
var under = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($55) {
                    return unwrap(dictNewtype1)(f(wrap(dictNewtype)($55)));
                };
            };
        };
    };
};
var un = function (dictNewtype) {
    return function (v) {
        return unwrap(dictNewtype);
    };
};
var traverse = function (dictFunctor) {
    return function (dictNewtype) {
        return function (v) {
            return function (f) {
                return function ($56) {
                    return Data_Functor.map(dictFunctor)(wrap(dictNewtype))(f(unwrap(dictNewtype)($56)));
                };
            };
        };
    };
};
var overF2 = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($57) {
                            return function ($58) {
                                return Data_Functor.map(dictFunctor1)(wrap(dictNewtype1))(Data_Function.on(f)(Data_Functor.map(dictFunctor)(unwrap(dictNewtype)))($57)($58));
                            };
                        };
                    };
                };
            };
        };
    };
};
var overF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($59) {
                            return Data_Functor.map(dictFunctor1)(wrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(unwrap(dictNewtype))($59)));
                        };
                    };
                };
            };
        };
    };
};
var over2 = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($60) {
                    return function ($61) {
                        return wrap(dictNewtype1)(Data_Function.on(f)(unwrap(dictNewtype))($60)($61));
                    };
                };
            };
        };
    };
};
var over = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($62) {
                    return wrap(dictNewtype1)(f(unwrap(dictNewtype)($62)));
                };
            };
        };
    };
};
var op = function (dictNewtype) {
    return un(dictNewtype);
};
var collect = function (dictFunctor) {
    return function (dictNewtype) {
        return function (v) {
            return function (f) {
                return function ($63) {
                    return wrap(dictNewtype)(f(Data_Functor.map(dictFunctor)(unwrap(dictNewtype))($63)));
                };
            };
        };
    };
};
var alaF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($64) {
                            return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(wrap(dictNewtype))($64)));
                        };
                    };
                };
            };
        };
    };
};
var ala = function (dictFunctor) {
    return function (dictNewtype) {
        return function (dictNewtype1) {
            return function (v) {
                return function (f) {
                    return Data_Functor.map(dictFunctor)(unwrap(dictNewtype))(f(wrap(dictNewtype1)));
                };
            };
        };
    };
};
module.exports = {
    Newtype: Newtype, 
    ala: ala, 
    alaF: alaF, 
    collect: collect, 
    op: op, 
    over: over, 
    over2: over2, 
    overF: overF, 
    overF2: overF2, 
    traverse: traverse, 
    un: un, 
    under: under, 
    under2: under2, 
    underF: underF, 
    underF2: underF2, 
    unwrap: unwrap, 
    wrap: wrap
};

},{"../Control.Semigroupoid":42,"../Data.Function":88,"../Data.Functor":91,"../Prelude":171}],119:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Plus = require("../Control.Plus");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var NonEmpty = (function () {
    function NonEmpty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    NonEmpty.create = function (value0) {
        return function (value1) {
            return new NonEmpty(value0, value1);
        };
    };
    return NonEmpty;
})();
var tail = function (v) {
    return v.value1;
};
var singleton = function (dictPlus) {
    return function (a) {
        return new NonEmpty(a, Control_Plus.empty(dictPlus));
    };
};
var showNonEmpty = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(NonEmpty " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var oneOf = function (dictAlternative) {
    return function (v) {
        return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Control_Applicative.pure(dictAlternative.Applicative0())(v.value0))(v.value1);
    };
};
var head = function (v) {
    return v.value0;
};
var functorNonEmpty = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return new NonEmpty(f(v.value0), Data_Functor.map(dictFunctor)(f)(v.value1));
        };
    });
};
var fromNonEmpty = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var foldl1 = function (dictFoldable) {
    return function (f) {
        return function (v) {
            return Data_Foldable.foldl(dictFoldable)(f)(v.value0)(v.value1);
        };
    };
};
var foldableNonEmpty = function (dictFoldable) {
    return new Data_Foldable.Foldable(function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return Data_Foldable.foldl(dictFoldable)(f)(f(b)(v.value0))(v.value1);
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return f(v.value0)(Data_Foldable.foldr(dictFoldable)(f)(b)(v.value1));
            };
        };
    });
};
var traversableNonEmpty = function (dictTraversable) {
    return new Data_Traversable.Traversable(function () {
        return foldableNonEmpty(dictTraversable.Foldable1());
    }, function () {
        return functorNonEmpty(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(v.value0))(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v.value1));
        };
    }, function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(v.value0)))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)(v.value1));
            };
        };
    });
};
var foldMap1 = function (dictSemigroup) {
    return function (dictFoldable) {
        return function (f) {
            return function (v) {
                return Data_Foldable.foldl(dictFoldable)(function (s) {
                    return function (a1) {
                        return Data_Semigroup.append(dictSemigroup)(s)(f(a1));
                    };
                })(f(v.value0))(v.value1);
            };
        };
    };
};
var fold1 = function (dictSemigroup) {
    return function (dictFoldable) {
        return foldMap1(dictSemigroup)(dictFoldable)(Control_Category.id(Control_Category.categoryFn));
    };
};
var eq1NonEmpty = function (dictEq1) {
    return new Data_Eq.Eq1(function (dictEq) {
        return function (v) {
            return function (v1) {
                return Data_Eq.eq(dictEq)(v.value0)(v1.value0) && Data_Eq.eq1(dictEq1)(dictEq)(v.value1)(v1.value1);
            };
        };
    });
};
var eqNonEmpty = function (dictEq1) {
    return function (dictEq) {
        return new Data_Eq.Eq(Data_Eq.eq1(eq1NonEmpty(dictEq1))(dictEq));
    };
};
var ord1NonEmpty = function (dictOrd1) {
    return new Data_Ord.Ord1(function () {
        return eq1NonEmpty(dictOrd1.Eq10());
    }, function (dictOrd) {
        return function (v) {
            return function (v1) {
                var v2 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                if (v2 instanceof Data_Ordering.EQ) {
                    return Data_Ord.compare1(dictOrd1)(dictOrd)(v.value1)(v1.value1);
                };
                return v2;
            };
        };
    });
};
var ordNonEmpty = function (dictOrd1) {
    return function (dictOrd) {
        return new Data_Ord.Ord(function () {
            return eqNonEmpty(dictOrd1.Eq10())(dictOrd.Eq0());
        }, Data_Ord.compare1(ord1NonEmpty(dictOrd1))(dictOrd));
    };
};
module.exports = {
    NonEmpty: NonEmpty, 
    fold1: fold1, 
    foldMap1: foldMap1, 
    foldl1: foldl1, 
    fromNonEmpty: fromNonEmpty, 
    head: head, 
    oneOf: oneOf, 
    singleton: singleton, 
    tail: tail, 
    showNonEmpty: showNonEmpty, 
    eqNonEmpty: eqNonEmpty, 
    eq1NonEmpty: eq1NonEmpty, 
    ordNonEmpty: ordNonEmpty, 
    ord1NonEmpty: ord1NonEmpty, 
    functorNonEmpty: functorNonEmpty, 
    foldableNonEmpty: foldableNonEmpty, 
    traversableNonEmpty: traversableNonEmpty
};

},{"../Control.Alt":4,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Control.Plus":41,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Traversable":144,"../Prelude":171}],120:[function(require,module,exports){
"use strict";

exports.unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x === y ? eq : gt;
        };
      };
    };
  };
};

},{}],121:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Ordering = require("../Data.Ordering");
var unsafeCompare = $foreign.unsafeCompareImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
module.exports = {
    unsafeCompare: unsafeCompare
};

},{"../Data.Ordering":124,"./foreign":120}],122:[function(require,module,exports){
"use strict";

exports.ordArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

},{}],123:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Ord_Unsafe = require("../Data.Ord.Unsafe");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Ord = function (Eq0, compare) {
    this.Eq0 = Eq0;
    this.compare = compare;
};
var Ord1 = function (Eq10, compare1) {
    this.Eq10 = Eq10;
    this.compare1 = compare1;
};
var ordVoid = new Ord(function () {
    return Data_Eq.eqVoid;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordUnit = new Ord(function () {
    return Data_Eq.eqUnit;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordString = new Ord(function () {
    return Data_Eq.eqString;
}, Data_Ord_Unsafe.unsafeCompare);
var ordOrdering = new Ord(function () {
    return Data_Ordering.eqOrdering;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Ordering.LT && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.EQ) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.GT && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        throw new Error("Failed pattern match at Data.Ord line 69, column 3 - line 69, column 21: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var ordNumber = new Ord(function () {
    return Data_Eq.eqNumber;
}, Data_Ord_Unsafe.unsafeCompare);
var ordInt = new Ord(function () {
    return Data_Eq.eqInt;
}, Data_Ord_Unsafe.unsafeCompare);
var ordChar = new Ord(function () {
    return Data_Eq.eqChar;
}, Data_Ord_Unsafe.unsafeCompare);
var ordBoolean = new Ord(function () {
    return Data_Eq.eqBoolean;
}, Data_Ord_Unsafe.unsafeCompare);
var compare1 = function (dict) {
    return dict.compare1;
};
var compare = function (dict) {
    return dict.compare;
};
var comparing = function (dictOrd) {
    return function (f) {
        return Data_Function.on(compare(dictOrd))(f);
    };
};
var greaterThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return true;
            };
            return false;
        };
    };
};
var greaterThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return false;
            };
            return true;
        };
    };
};
var signum = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $33 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($33) {
                return Data_Semiring.one(dictRing.Semiring0());
            };
            return Data_Ring.negate(dictRing)(Data_Semiring.one(dictRing.Semiring0()));
        };
    };
};
var lessThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return true;
            };
            return false;
        };
    };
};
var lessThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return false;
            };
            return true;
        };
    };
};
var max = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return y;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return x;
            };
            throw new Error("Failed pattern match at Data.Ord line 123, column 3 - line 126, column 12: " + [ v.constructor.name ]);
        };
    };
};
var min = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return x;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return y;
            };
            throw new Error("Failed pattern match at Data.Ord line 114, column 3 - line 117, column 12: " + [ v.constructor.name ]);
        };
    };
};
var ordArray = function (dictOrd) {
    return new Ord(function () {
        return Data_Eq.eqArray(dictOrd.Eq0());
    }, (function () {
        var toDelta = function (x) {
            return function (y) {
                var v = compare(dictOrd)(x)(y);
                if (v instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if (v instanceof Data_Ordering.LT) {
                    return 1;
                };
                if (v instanceof Data_Ordering.GT) {
                    return -1 | 0;
                };
                throw new Error("Failed pattern match at Data.Ord line 61, column 7 - line 66, column 1: " + [ v.constructor.name ]);
            };
        };
        return function (xs) {
            return function (ys) {
                return compare(ordInt)(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
            };
        };
    })());
};
var ord1Array = new Ord1(function () {
    return Data_Eq.eq1Array;
}, function (dictOrd) {
    return compare(ordArray(dictOrd));
});
var clamp = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                return min(dictOrd)(hi)(max(dictOrd)(low)(x));
            };
        };
    };
};
var between = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                if (lessThan(dictOrd)(x)(low)) {
                    return false;
                };
                if (greaterThan(dictOrd)(x)(hi)) {
                    return false;
                };
                return true;
            };
        };
    };
};
var abs = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $42 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($42) {
                return x;
            };
            return Data_Ring.negate(dictRing)(x);
        };
    };
};
module.exports = {
    Ord: Ord, 
    Ord1: Ord1, 
    abs: abs, 
    between: between, 
    clamp: clamp, 
    compare: compare, 
    compare1: compare1, 
    comparing: comparing, 
    greaterThan: greaterThan, 
    greaterThanOrEq: greaterThanOrEq, 
    lessThan: lessThan, 
    lessThanOrEq: lessThanOrEq, 
    max: max, 
    min: min, 
    signum: signum, 
    ordBoolean: ordBoolean, 
    ordInt: ordInt, 
    ordNumber: ordNumber, 
    ordString: ordString, 
    ordChar: ordChar, 
    ordUnit: ordUnit, 
    ordVoid: ordVoid, 
    ordArray: ordArray, 
    ordOrdering: ordOrdering, 
    ord1Array: ord1Array
};

},{"../Data.Eq":78,"../Data.Function":88,"../Data.Ord.Unsafe":121,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semiring":130,"../Data.Unit":149,"../Data.Void":150,"./foreign":122}],124:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var showOrdering = new Data_Show.Show(function (v) {
    if (v instanceof LT) {
        return "LT";
    };
    if (v instanceof GT) {
        return "GT";
    };
    if (v instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Data.Ordering line 27, column 3 - line 28, column 3: " + [ v.constructor.name ]);
});
var semigroupOrdering = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        if (v instanceof EQ) {
            return v1;
        };
        throw new Error("Failed pattern match at Data.Ordering line 22, column 3 - line 22, column 19: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invert = function (v) {
    if (v instanceof GT) {
        return LT.value;
    };
    if (v instanceof EQ) {
        return EQ.value;
    };
    if (v instanceof LT) {
        return GT.value;
    };
    throw new Error("Failed pattern match at Data.Ordering line 34, column 1 - line 34, column 15: " + [ v.constructor.name ]);
};
var eqOrdering = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return true;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return true;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return true;
        };
        return false;
    };
});
module.exports = {
    LT: LT, 
    GT: GT, 
    EQ: EQ, 
    invert: invert, 
    eqOrdering: eqOrdering, 
    semigroupOrdering: semigroupOrdering, 
    showOrdering: showOrdering
};

},{"../Data.Eq":78,"../Data.Semigroup":128,"../Data.Show":132}],125:[function(require,module,exports){
"use strict";

exports.intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

},{}],126:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Ring = function (Semiring0, sub) {
    this.Semiring0 = Semiring0;
    this.sub = sub;
};
var sub = function (dict) {
    return dict.sub;
};
var ringUnit = new Ring(function () {
    return Data_Semiring.semiringUnit;
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var ringNumber = new Ring(function () {
    return Data_Semiring.semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return Data_Semiring.semiringInt;
}, $foreign.intSub);
var ringFn = function (dictRing) {
    return new Ring(function () {
        return Data_Semiring.semiringFn(dictRing.Semiring0());
    }, function (f) {
        return function (g) {
            return function (x) {
                return sub(dictRing)(f(x))(g(x));
            };
        };
    });
};
var negate = function (dictRing) {
    return function (a) {
        return sub(dictRing)(Data_Semiring.zero(dictRing.Semiring0()))(a);
    };
};
module.exports = {
    Ring: Ring, 
    negate: negate, 
    sub: sub, 
    ringInt: ringInt, 
    ringNumber: ringNumber, 
    ringUnit: ringUnit, 
    ringFn: ringFn
};

},{"../Data.Semiring":130,"../Data.Unit":149,"./foreign":125}],127:[function(require,module,exports){
"use strict";

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function (xs) {
  return function (ys) {
    if (xs.length === 0) return ys;
    if (ys.length === 0) return xs;
    return xs.concat(ys);
  };
};

},{}],128:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
var Semigroup = function (append) {
    this.append = append;
};
var semigroupVoid = new Semigroup(function (v) {
    return Data_Void.absurd;
});
var semigroupUnit = new Semigroup(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupArray = new Semigroup($foreign.concatArray);
var append = function (dict) {
    return dict.append;
};
var semigroupFn = function (dictSemigroup) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return append(dictSemigroup)(f(x))(g(x));
            };
        };
    });
};
module.exports = {
    Semigroup: Semigroup, 
    append: append, 
    semigroupString: semigroupString, 
    semigroupUnit: semigroupUnit, 
    semigroupVoid: semigroupVoid, 
    semigroupFn: semigroupFn, 
    semigroupArray: semigroupArray
};

},{"../Data.Unit":149,"../Data.Void":150,"./foreign":127}],129:[function(require,module,exports){
"use strict";

exports.intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

},{}],130:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Unit = require("../Data.Unit");
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var zero = function (dict) {
    return dict.zero;
};
var semiringUnit = new Semiring(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, Data_Unit.unit);
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var one = function (dict) {
    return dict.one;
};
var mul = function (dict) {
    return dict.mul;
};
var add = function (dict) {
    return dict.add;
};
var semiringFn = function (dictSemiring) {
    return new Semiring(function (f) {
        return function (g) {
            return function (x) {
                return add(dictSemiring)(f(x))(g(x));
            };
        };
    }, function (f) {
        return function (g) {
            return function (x) {
                return mul(dictSemiring)(f(x))(g(x));
            };
        };
    }, function (v) {
        return one(dictSemiring);
    }, function (v) {
        return zero(dictSemiring);
    });
};
module.exports = {
    Semiring: Semiring, 
    add: add, 
    mul: mul, 
    one: one, 
    zero: zero, 
    semiringInt: semiringInt, 
    semiringNumber: semiringNumber, 
    semiringFn: semiringFn, 
    semiringUnit: semiringUnit
};

},{"../Data.Unit":149,"./foreign":129}],131:[function(require,module,exports){
"use strict";

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

exports.showCharImpl = function (c) {
  var code = c.charCodeAt(0);
  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  var l = s.length;
  return "\"" + s.replace(
    /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
    function (c, i) {
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;
        case "\x07": return "\\a";
        case "\b": return "\\b";
        case "\f": return "\\f";
        case "\n": return "\\n";
        case "\r": return "\\r";
        case "\t": return "\\t";
        case "\v": return "\\v";
      }
      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }
  ) + "\"";
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

},{}],132:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Show = function (show) {
    this.show = show;
};
var showString = new Show($foreign.showStringImpl);
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (v) {
    if (v) {
        return "true";
    };
    if (!v) {
        return "false";
    };
    throw new Error("Failed pattern match at Data.Show line 13, column 3 - line 14, column 3: " + [ v.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
};
module.exports = {
    Show: Show, 
    show: show, 
    showBoolean: showBoolean, 
    showInt: showInt, 
    showNumber: showNumber, 
    showChar: showChar, 
    showString: showString, 
    showArray: showArray
};

},{"./foreign":131}],133:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_MonadPlus = require("../Control.MonadPlus");
var Control_MonadZero = require("../Control.MonadZero");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Prelude = require("../Prelude");
var RegexFlags = (function () {
    function RegexFlags(value0) {
        this.value0 = value0;
    };
    RegexFlags.create = function (value0) {
        return new RegexFlags(value0);
    };
    return RegexFlags;
})();
var unicode = new RegexFlags({
    global: false, 
    ignoreCase: false, 
    multiline: false, 
    sticky: false, 
    unicode: true
});
var sticky = new RegexFlags({
    global: false, 
    ignoreCase: false, 
    multiline: false, 
    sticky: true, 
    unicode: false
});
var showRegexFlags = new Data_Show.Show(function (v) {
    var usedFlags = Data_Semigroup.append(Data_Semigroup.semigroupArray)([  ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.global))("global"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.ignoreCase))("ignoreCase"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.multiline))("multiline"))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.sticky))("sticky"))(Data_Functor.voidLeft(Data_Functor.functorArray)(Control_MonadZero.guard(Control_MonadZero.monadZeroArray)(v.value0.unicode))("unicode"))))));
    var $6 = Data_Eq.eq(Data_Eq.eqArray(Data_Eq.eqString))(usedFlags)([  ]);
    if ($6) {
        return "noFlags";
    };
    return "(" + (Data_String.joinWith(" <> ")(usedFlags) + ")");
});
var semigroupRegexFlags = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return new RegexFlags({
            global: v.value0.global || v1.value0.global, 
            ignoreCase: v.value0.ignoreCase || v1.value0.ignoreCase, 
            multiline: v.value0.multiline || v1.value0.multiline, 
            sticky: v.value0.sticky || v1.value0.sticky, 
            unicode: v.value0.unicode || v1.value0.unicode
        });
    };
});
var noFlags = new RegexFlags({
    global: false, 
    ignoreCase: false, 
    multiline: false, 
    sticky: false, 
    unicode: false
});
var multiline = new RegexFlags({
    global: false, 
    ignoreCase: false, 
    multiline: true, 
    sticky: false, 
    unicode: false
});
var monoidRegexFlags = new Data_Monoid.Monoid(function () {
    return semigroupRegexFlags;
}, noFlags);
var ignoreCase = new RegexFlags({
    global: false, 
    ignoreCase: true, 
    multiline: false, 
    sticky: false, 
    unicode: false
});
var global = new RegexFlags({
    global: true, 
    ignoreCase: false, 
    multiline: false, 
    sticky: false, 
    unicode: false
});
var eqRegexFlags = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v.value0.global === v1.value0.global && (v.value0.ignoreCase === v1.value0.ignoreCase && (v.value0.multiline === v1.value0.multiline && (v.value0.sticky === v1.value0.sticky && v.value0.unicode === v1.value0.unicode)));
    };
});
module.exports = {
    RegexFlags: RegexFlags, 
    global: global, 
    ignoreCase: ignoreCase, 
    multiline: multiline, 
    noFlags: noFlags, 
    sticky: sticky, 
    unicode: unicode, 
    semigroupRegexFlags: semigroupRegexFlags, 
    monoidRegexFlags: monoidRegexFlags, 
    eqRegexFlags: eqRegexFlags, 
    showRegexFlags: showRegexFlags
};

},{"../Control.MonadPlus":39,"../Control.MonadZero":40,"../Data.Eq":78,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.Monoid":116,"../Data.Semigroup":128,"../Data.Show":132,"../Data.String":139,"../Prelude":171}],134:[function(require,module,exports){
"use strict";

exports["showRegex'"] = function (r) {
  return "" + r;
};

exports["regex'"] = function (left) {
  return function (right) {
    return function (s1) {
      return function (s2) {
        try {
          return right(new RegExp(s1, s2));
        } catch (e) {
          return left(e.message);
        }
      };
    };
  };
};

exports.source = function (r) {
  return r.source;
};

exports["flags'"] = function (r) {
  return {
    multiline: r.multiline,
    ignoreCase: r.ignoreCase,
    global: r.global,
    sticky: !!r.sticky,
    unicode: !!r.unicode
  };
};

exports.test = function (r) {
  return function (s) {
    var lastIndex = r.lastIndex;
    var result = r.test(s);
    r.lastIndex = lastIndex;
    return result;
  };
};

exports._match = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var m = s.match(r);
        if (m == null) {
          return nothing;
        } else {
          var list = [];
          for (var i = 0; i < m.length; i++) {
            list.push(m[i] == null ? nothing : just(m[i]));
          }
          return just(list);
        }
      };
    };
  };
};

exports.replace = function (r) {
  return function (s1) {
    return function (s2) {
      return s2.replace(r, s1);
    };
  };
};

exports["replace'"] = function (r) {
  return function (f) {
    return function (s2) {
      return s2.replace(r, function (match) {
        return f(match)(Array.prototype.splice.call(arguments, 1, arguments.length - 3));
      });
    };
  };
};

exports._search = function (just) {
  return function (nothing) {
    return function (r) {
      return function (s) {
        var result = s.search(r);
        return result === -1 ? nothing : just(result);
      };
    };
  };
};

exports.split = function (r) {
  return function (s) {
    return s.split(r);
  };
};

},{}],135:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags");
var Prelude = require("../Prelude");
var showRegex = new Data_Show.Show($foreign["showRegex'"]);
var search = $foreign._search(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var renderFlags = function (v) {
    return (function () {
        if (v.value0.global) {
            return "g";
        };
        return "";
    })() + ((function () {
        if (v.value0.ignoreCase) {
            return "i";
        };
        return "";
    })() + ((function () {
        if (v.value0.multiline) {
            return "m";
        };
        return "";
    })() + ((function () {
        if (v.value0.sticky) {
            return "y";
        };
        return "";
    })() + (function () {
        if (v.value0.unicode) {
            return "u";
        };
        return "";
    })())));
};
var regex = function (s) {
    return function (f) {
        return $foreign["regex'"](Data_Either.Left.create)(Data_Either.Right.create)(s)(renderFlags(f));
    };
};
var parseFlags = function (s) {
    return new Data_String_Regex_Flags.RegexFlags({
        global: Data_String.contains("g")(s), 
        ignoreCase: Data_String.contains("i")(s), 
        multiline: Data_String.contains("m")(s), 
        sticky: Data_String.contains("y")(s), 
        unicode: Data_String.contains("u")(s)
    });
};
var match = $foreign._match(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var flags = function ($8) {
    return Data_String_Regex_Flags.RegexFlags.create($foreign["flags'"]($8));
};
module.exports = {
    flags: flags, 
    match: match, 
    parseFlags: parseFlags, 
    regex: regex, 
    renderFlags: renderFlags, 
    search: search, 
    showRegex: showRegex, 
    replace: $foreign.replace, 
    "replace'": $foreign["replace'"], 
    source: $foreign.source, 
    split: $foreign.split, 
    test: $foreign.test
};

},{"../Control.Semigroupoid":42,"../Data.Either":75,"../Data.Function":88,"../Data.Maybe":109,"../Data.Semigroup":128,"../Data.Show":132,"../Data.String":139,"../Data.String.Regex.Flags":133,"../Prelude":171,"./foreign":134}],136:[function(require,module,exports){
"use strict";

exports.charCodeAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charCodeAt(i);
    throw new Error("Data.String.Unsafe.charCodeAt: Invalid index.");
  };
};

exports.charAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

exports.char = function (s) {
  if (s.length === 1) return s.charAt(0);
  throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
};

},{}],137:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    "char": $foreign["char"], 
    charAt: $foreign.charAt, 
    charCodeAt: $foreign.charCodeAt
};

},{"./foreign":136}],138:[function(require,module,exports){
"use strict";

exports._charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

exports.singleton = function (c) {
  return c;
};

exports._charCodeAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charCodeAt(i)) : nothing;
      };
    };
  };
};

exports._toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

exports.fromCharArray = function (a) {
  return a.join("");
};

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_indexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports._lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_lastIndexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports.length = function (s) {
  return s.length;
};

exports._localeCompare = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (s1) {
        return function (s2) {
          var result = s1.localeCompare(s2);
          return result < 0 ? lt : result > 0 ? gt : eq;
        };
      };
    };
  };
};

exports.replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};

exports.replaceAll = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(new RegExp(s1.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "g"), s2);
    };
  };
};

exports.take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

exports.count = function (p) {
  return function (s) {
    var i = 0;
    while (i < s.length && p(s.charAt(i))) i++;
    return i;
  };
};

exports.split = function (sep) {
  return function (s) {
    return s.split(sep);
  };
};

exports._splitAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ?
               just({ before: s.substring(0, i), after: s.substring(i) }) :
               nothing;
      };
    };
  };
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.toLower = function (s) {
  return s.toLowerCase();
};

exports.toUpper = function (s) {
  return s.toUpperCase();
};

exports.trim = function (s) {
  return s.trim();
};

exports.joinWith = function (s) {
  return function (xs) {
    return xs.join(s);
  };
};

},{}],139:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String_Unsafe = require("../Data.String.Unsafe");
var Prelude = require("../Prelude");
var Replacement = function (x) {
    return x;
};
var Pattern = function (x) {
    return x;
};
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v), 
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign._toChar(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.count(p)(s))(s);
    };
};
var splitAt = $foreign._splitAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var showReplacement = new Data_Show.Show(function (v) {
    return "(Replacement " + (v + ")");
});
var showPattern = new Data_Show.Show(function (v) {
    return "(Pattern " + (v + ")");
});
var $$null = function (s) {
    return s === "";
};
var newtypeReplacement = new Data_Newtype.Newtype(function (n) {
    return n;
}, Replacement);
var newtypePattern = new Data_Newtype.Newtype(function (n) {
    return n;
}, Pattern);
var localeCompare = $foreign._localeCompare(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign._lastIndexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (v) {
    return function (str) {
        var v1 = lastIndexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === ($foreign.length(str) - $foreign.length(v) | 0)) {
            return Data_Maybe.Just.create($foreign.take(v1.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign._indexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (v) {
    return function (str) {
        var v1 = indexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === 0) {
            return Data_Maybe.Just.create($foreign.drop($foreign.length(v))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var eqReplacement = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordReplacement = new Data_Ord.Ord(function () {
    return eqReplacement;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var eqPattern = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordPattern = new Data_Ord.Ord(function () {
    return eqPattern;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.count(p)(s))(s);
    };
};
var contains = function (pat) {
    return function ($48) {
        return Data_Maybe.isJust(indexOf(pat)($48));
    };
};
var charCodeAt = $foreign._charCodeAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var charAt = $foreign._charAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    Pattern: Pattern, 
    Replacement: Replacement, 
    charAt: charAt, 
    charCodeAt: charCodeAt, 
    contains: contains, 
    dropWhile: dropWhile, 
    indexOf: indexOf, 
    "indexOf'": indexOf$prime, 
    lastIndexOf: lastIndexOf, 
    "lastIndexOf'": lastIndexOf$prime, 
    localeCompare: localeCompare, 
    "null": $$null, 
    splitAt: splitAt, 
    stripPrefix: stripPrefix, 
    stripSuffix: stripSuffix, 
    takeWhile: takeWhile, 
    toChar: toChar, 
    uncons: uncons, 
    eqPattern: eqPattern, 
    ordPattern: ordPattern, 
    newtypePattern: newtypePattern, 
    showPattern: showPattern, 
    eqReplacement: eqReplacement, 
    ordReplacement: ordReplacement, 
    newtypeReplacement: newtypeReplacement, 
    showReplacement: showReplacement, 
    count: $foreign.count, 
    drop: $foreign.drop, 
    fromCharArray: $foreign.fromCharArray, 
    joinWith: $foreign.joinWith, 
    length: $foreign.length, 
    replace: $foreign.replace, 
    replaceAll: $foreign.replaceAll, 
    singleton: $foreign.singleton, 
    split: $foreign.split, 
    take: $foreign.take, 
    toCharArray: $foreign.toCharArray, 
    toLower: $foreign.toLower, 
    toUpper: $foreign.toUpper, 
    trim: $foreign.trim
};

},{"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.Function":88,"../Data.Maybe":109,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.String.Unsafe":137,"../Prelude":171,"./foreign":138}],140:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Second = function (x) {
    return x;
};
var Minute = function (x) {
    return x;
};
var Millisecond = function (x) {
    return x;
};
var Hour = function (x) {
    return x;
};
var showSecond = new Data_Show.Show(function (v) {
    return "(Second " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showMinute = new Data_Show.Show(function (v) {
    return "(Minute " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showMillisecond = new Data_Show.Show(function (v) {
    return "(Millisecond " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showHour = new Data_Show.Show(function (v) {
    return "(Hour " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var ordSecond = Data_Ord.ordInt;
var ordMinute = Data_Ord.ordInt;
var ordMillisecond = Data_Ord.ordInt;
var ordHour = Data_Ord.ordInt;
var genericSecond = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Second" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Second))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Second", [ {
        sigConstructor: "Data.Time.Component.Second", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Second", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericMinute = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Minute" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Minute))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Minute", [ {
        sigConstructor: "Data.Time.Component.Minute", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Minute", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericMillisecond = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Millisecond" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Millisecond))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Millisecond", [ {
        sigConstructor: "Data.Time.Component.Millisecond", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Millisecond", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericHour = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Hour" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Hour))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Hour", [ {
        sigConstructor: "Data.Time.Component.Hour", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Hour", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var eqSecond = Data_Eq.eqInt;
var eqMinute = Data_Eq.eqInt;
var eqMillisecond = Data_Eq.eqInt;
var eqHour = Data_Eq.eqInt;
var boundedSecond = new Data_Bounded.Bounded(function () {
    return ordSecond;
}, 0, 59);
var boundedMinute = new Data_Bounded.Bounded(function () {
    return ordMinute;
}, 0, 59);
var boundedMillisecond = new Data_Bounded.Bounded(function () {
    return ordMillisecond;
}, 0, 999);
var boundedHour = new Data_Bounded.Bounded(function () {
    return ordHour;
}, 0, 23);
var boundedEnumSecond = new Data_Enum.BoundedEnum(function () {
    return boundedSecond;
}, function () {
    return enumSecond;
}, 60, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 59) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 96, column 3 - line 98, column 26: " + [ n.constructor.name ]);
});
var enumSecond = new Data_Enum.Enum(function () {
    return ordSecond;
}, function ($64) {
    return Data_Enum.toEnum(boundedEnumSecond)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumSecond)($64)));
}, function ($65) {
    return Data_Enum.toEnum(boundedEnumSecond)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumSecond)($65)));
});
var boundedEnumMinute = new Data_Enum.BoundedEnum(function () {
    return boundedMinute;
}, function () {
    return enumMinute;
}, 60, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 59) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 66, column 3 - line 68, column 26: " + [ n.constructor.name ]);
});
var enumMinute = new Data_Enum.Enum(function () {
    return ordMinute;
}, function ($66) {
    return Data_Enum.toEnum(boundedEnumMinute)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMinute)($66)));
}, function ($67) {
    return Data_Enum.toEnum(boundedEnumMinute)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMinute)($67)));
});
var boundedEnumMillisecond = new Data_Enum.BoundedEnum(function () {
    return boundedMillisecond;
}, function () {
    return enumMillisecond;
}, 1000, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 999) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 127, column 3 - line 129, column 26: " + [ n.constructor.name ]);
});
var enumMillisecond = new Data_Enum.Enum(function () {
    return ordMillisecond;
}, function ($68) {
    return Data_Enum.toEnum(boundedEnumMillisecond)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMillisecond)($68)));
}, function ($69) {
    return Data_Enum.toEnum(boundedEnumMillisecond)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMillisecond)($69)));
});
var boundedEnumHour = new Data_Enum.BoundedEnum(function () {
    return boundedHour;
}, function () {
    return enumHour;
}, 24, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 23) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 36, column 3 - line 38, column 26: " + [ n.constructor.name ]);
});
var enumHour = new Data_Enum.Enum(function () {
    return ordHour;
}, function ($70) {
    return Data_Enum.toEnum(boundedEnumHour)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumHour)($70)));
}, function ($71) {
    return Data_Enum.toEnum(boundedEnumHour)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumHour)($71)));
});
module.exports = {
    eqHour: eqHour, 
    ordHour: ordHour, 
    genericHour: genericHour, 
    boundedHour: boundedHour, 
    enumHour: enumHour, 
    boundedEnumHour: boundedEnumHour, 
    showHour: showHour, 
    eqMinute: eqMinute, 
    ordMinute: ordMinute, 
    genericMinute: genericMinute, 
    boundedMinute: boundedMinute, 
    enumMinute: enumMinute, 
    boundedEnumMinute: boundedEnumMinute, 
    showMinute: showMinute, 
    eqSecond: eqSecond, 
    ordSecond: ordSecond, 
    genericSecond: genericSecond, 
    boundedSecond: boundedSecond, 
    enumSecond: enumSecond, 
    boundedEnumSecond: boundedEnumSecond, 
    showSecond: showSecond, 
    eqMillisecond: eqMillisecond, 
    ordMillisecond: ordMillisecond, 
    genericMillisecond: genericMillisecond, 
    boundedMillisecond: boundedMillisecond, 
    enumMillisecond: enumMillisecond, 
    boundedEnumMillisecond: boundedEnumMillisecond, 
    showMillisecond: showMillisecond
};

},{"../Control.Apply":8,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Bounded":66,"../Data.Enum":76,"../Data.Eq":78,"../Data.Generic":93,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Unit":149,"../Prelude":171}],141:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Seconds = function (x) {
    return x;
};
var Minutes = function (x) {
    return x;
};
var Milliseconds = function (x) {
    return x;
};
var Hours = function (x) {
    return x;
};
var Days = function (x) {
    return x;
};
var Duration = function (fromDuration, toDuration) {
    this.fromDuration = fromDuration;
    this.toDuration = toDuration;
};
var toDuration = function (dict) {
    return dict.toDuration;
};
var showSeconds = new Data_Show.Show(function (v) {
    return "(Seconds " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showMinutes = new Data_Show.Show(function (v) {
    return "(Minutes " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showMilliseconds = new Data_Show.Show(function (v) {
    return "(Milliseconds " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showHours = new Data_Show.Show(function (v) {
    return "(Hours " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var showDays = new Data_Show.Show(function (v) {
    return "(Days " + (Data_Show.show(Data_Show.showNumber)(v) + ")");
});
var semiringSeconds = Data_Semiring.semiringNumber;
var semiringMinutes = Data_Semiring.semiringNumber;
var semiringMilliseconds = Data_Semiring.semiringNumber;
var semiringHours = Data_Semiring.semiringNumber;
var semiringDays = Data_Semiring.semiringNumber;
var ringSeconds = Data_Ring.ringNumber;
var ringMinutes = Data_Ring.ringNumber;
var ringMilliseconds = Data_Ring.ringNumber;
var ringHours = Data_Ring.ringNumber;
var ringDays = Data_Ring.ringNumber;
var ordSeconds = Data_Ord.ordNumber;
var ordMinutes = Data_Ord.ordNumber;
var ordMilliseconds = Data_Ord.ordNumber;
var ordHours = Data_Ord.ordNumber;
var ordDays = Data_Ord.ordNumber;
var newtypeSeconds = new Data_Newtype.Newtype(function (n) {
    return n;
}, Seconds);
var newtypeMinutes = new Data_Newtype.Newtype(function (n) {
    return n;
}, Minutes);
var newtypeMilliseconds = new Data_Newtype.Newtype(function (n) {
    return n;
}, Milliseconds);
var newtypeHours = new Data_Newtype.Newtype(function (n) {
    return n;
}, Hours);
var newtypeDays = new Data_Newtype.Newtype(function (n) {
    return n;
}, Days);
var genericSeconds = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Duration.Seconds" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Seconds))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Duration.Seconds", [ {
        sigConstructor: "Data.Time.Duration.Seconds", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Duration.Seconds", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
    } ]);
});
var genericMinutes = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Duration.Minutes" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Minutes))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Duration.Minutes", [ {
        sigConstructor: "Data.Time.Duration.Minutes", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Duration.Minutes", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
    } ]);
});
var genericMilliseconds = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Duration.Milliseconds" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Milliseconds))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Duration.Milliseconds", [ {
        sigConstructor: "Data.Time.Duration.Milliseconds", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Duration.Milliseconds", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
    } ]);
});
var genericHours = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Duration.Hours" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Hours))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Duration.Hours", [ {
        sigConstructor: "Data.Time.Duration.Hours", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Duration.Hours", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
    } ]);
});
var genericDays = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Duration.Days" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Days))(Data_Generic.fromSpine(Data_Generic.genericNumber)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Duration.Days", [ {
        sigConstructor: "Data.Time.Duration.Days", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericNumber)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Duration.Days", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericNumber)(v);
    } ]);
});
var fromDuration = function (dict) {
    return dict.fromDuration;
};
var eqSeconds = Data_Eq.eqNumber;
var eqMinutes = Data_Eq.eqNumber;
var eqMilliseconds = Data_Eq.eqNumber;
var eqHours = Data_Eq.eqNumber;
var eqDays = Data_Eq.eqNumber;
var durationSeconds = new Duration(Data_Newtype.over(newtypeSeconds)(newtypeMilliseconds)(Seconds)(function (v) {
    return v * 1000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeSeconds)(Milliseconds)(function (v) {
    return v / 1000.0;
}));
var durationMinutes = new Duration(Data_Newtype.over(newtypeMinutes)(newtypeMilliseconds)(Minutes)(function (v) {
    return v * 60000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeMinutes)(Milliseconds)(function (v) {
    return v / 60000.0;
}));
var durationMilliseconds = new Duration(Control_Category.id(Control_Category.categoryFn), Control_Category.id(Control_Category.categoryFn));
var durationHours = new Duration(Data_Newtype.over(newtypeHours)(newtypeMilliseconds)(Hours)(function (v) {
    return v * 3600000.0;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeHours)(Milliseconds)(function (v) {
    return v / 3600000.0;
}));
var durationDays = new Duration(Data_Newtype.over(newtypeDays)(newtypeMilliseconds)(Days)(function (v) {
    return v * 8.64e7;
}), Data_Newtype.over(newtypeMilliseconds)(newtypeDays)(Milliseconds)(function (v) {
    return v / 8.64e7;
}));
var convertDuration = function (dictDuration) {
    return function (dictDuration1) {
        return function ($80) {
            return toDuration(dictDuration1)(fromDuration(dictDuration)($80));
        };
    };
};
module.exports = {
    Days: Days, 
    Hours: Hours, 
    Milliseconds: Milliseconds, 
    Minutes: Minutes, 
    Seconds: Seconds, 
    Duration: Duration, 
    convertDuration: convertDuration, 
    fromDuration: fromDuration, 
    toDuration: toDuration, 
    newtypeMilliseconds: newtypeMilliseconds, 
    genericMilliseconds: genericMilliseconds, 
    eqMilliseconds: eqMilliseconds, 
    ordMilliseconds: ordMilliseconds, 
    semiringMilliseconds: semiringMilliseconds, 
    ringMilliseconds: ringMilliseconds, 
    showMilliseconds: showMilliseconds, 
    newtypeSeconds: newtypeSeconds, 
    genericSeconds: genericSeconds, 
    eqSeconds: eqSeconds, 
    ordSeconds: ordSeconds, 
    semiringSeconds: semiringSeconds, 
    ringSeconds: ringSeconds, 
    showSeconds: showSeconds, 
    newtypeMinutes: newtypeMinutes, 
    genericMinutes: genericMinutes, 
    eqMinutes: eqMinutes, 
    ordMinutes: ordMinutes, 
    semiringMinutes: semiringMinutes, 
    ringMinutes: ringMinutes, 
    showMinutes: showMinutes, 
    newtypeHours: newtypeHours, 
    genericHours: genericHours, 
    eqHours: eqHours, 
    ordHours: ordHours, 
    semiringHours: semiringHours, 
    ringHours: ringHours, 
    showHours: showHours, 
    newtypeDays: newtypeDays, 
    genericDays: genericDays, 
    eqDays: eqDays, 
    ordDays: ordDays, 
    semiringDays: semiringDays, 
    ringDays: ringDays, 
    showDays: showDays, 
    durationMilliseconds: durationMilliseconds, 
    durationSeconds: durationSeconds, 
    durationMinutes: durationMinutes, 
    durationHours: durationHours, 
    durationDays: durationDays
};

},{"../Control.Apply":8,"../Control.Category":13,"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Generic":93,"../Data.Maybe":109,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Unit":149,"../Prelude":171}],142:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Apply = require("../Control.Apply");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Time_Component = require("../Data.Time.Component");
var Data_Time_Duration = require("../Data.Time.Duration");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Time = (function () {
    function Time(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Time.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Time(value0, value1, value2, value3);
                };
            };
        };
    };
    return Time;
})();
var showTime = new Data_Show.Show(function (v) {
    return "(Time " + (Data_Show.show(Data_Time_Component.showHour)(v.value0) + (" " + (Data_Show.show(Data_Time_Component.showMinute)(v.value1) + (" " + (Data_Show.show(Data_Time_Component.showSecond)(v.value2) + (" " + (Data_Show.show(Data_Time_Component.showMillisecond)(v.value3) + ")")))))));
});
var setSecond = function (s) {
    return function (v) {
        return new Time(v.value0, v.value1, s, v.value3);
    };
};
var setMinute = function (m) {
    return function (v) {
        return new Time(v.value0, m, v.value2, v.value3);
    };
};
var setMillisecond = function (ms) {
    return function (v) {
        return new Time(v.value0, v.value1, v.value2, ms);
    };
};
var setHour = function (h) {
    return function (v) {
        return new Time(h, v.value1, v.value2, v.value3);
    };
};
var second = function (v) {
    return v.value2;
};
var minute = function (v) {
    return v.value1;
};
var millisecond = function (v) {
    return v.value3;
};
var millisToTime = function (v) {
    var hours = $$Math.floor(v / 3600000.0);
    var minutes = $$Math.floor((v - hours * 3600000.0) / 60000.0);
    var seconds = $$Math.floor((v - (hours * 3600000.0 + minutes * 60000.0)) / 1000.0);
    var milliseconds = v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0);
    return Data_Maybe.fromJust()(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Time.create)(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)(Data_Int.floor(hours))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)(Data_Int.floor(minutes))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)(Data_Int.floor(seconds))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Int.floor(milliseconds))));
};
var hour = function (v) {
    return v.value0;
};
var timeToMillis = function (t) {
    return Data_Time_Duration.Milliseconds(3600000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(hour(t))) + 60000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(minute(t))) + 1000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(second(t))) + Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(millisecond(t))));
};
var genericTime = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Time" && v.value1.length === 4)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Time.create))(Data_Generic.fromSpine(Data_Time_Component.genericHour)(v["value1"][0](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Time_Component.genericMinute)(v["value1"][1](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Time_Component.genericSecond)(v["value1"][2](Data_Unit.unit))))(Data_Generic.fromSpine(Data_Time_Component.genericMillisecond)(v["value1"][3](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Time", [ {
        sigConstructor: "Data.Time.Time", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Time_Component.genericHour)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Time_Component.genericMinute)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Time_Component.genericSecond)(Data_Generic.anyProxy);
        }, function ($dollarq1) {
            return Data_Generic.toSignature(Data_Time_Component.genericMillisecond)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Time", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Time_Component.genericHour)(v.value0);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Time_Component.genericMinute)(v.value1);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Time_Component.genericSecond)(v.value2);
    }, function ($dollarq) {
        return Data_Generic.toSpine(Data_Time_Component.genericMillisecond)(v.value3);
    } ]);
});
var eqTime = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Time_Component.eqHour)(x.value0)(y.value0) && Data_Eq.eq(Data_Time_Component.eqMinute)(x.value1)(y.value1) && Data_Eq.eq(Data_Time_Component.eqSecond)(x.value2)(y.value2) && Data_Eq.eq(Data_Time_Component.eqMillisecond)(x.value3)(y.value3);
    };
});
var ordTime = new Data_Ord.Ord(function () {
    return eqTime;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Time_Component.ordHour)(x.value0)(y.value0);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v1 = Data_Ord.compare(Data_Time_Component.ordMinute)(x.value1)(y.value1);
        if (v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        var v2 = Data_Ord.compare(Data_Time_Component.ordSecond)(x.value2)(y.value2);
        if (v2 instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v2 instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Time_Component.ordMillisecond)(x.value3)(y.value3);
    };
});
var diff = function (dictDuration) {
    return function (t1) {
        return function (t2) {
            return Data_Time_Duration.toDuration(dictDuration)(Data_Ring.sub(Data_Time_Duration.ringMilliseconds)(timeToMillis(t1))(timeToMillis(t2)));
        };
    };
};
var boundedTime = new Data_Bounded.Bounded(function () {
    return ordTime;
}, new Time(Data_Bounded.bottom(Data_Time_Component.boundedHour), Data_Bounded.bottom(Data_Time_Component.boundedMinute), Data_Bounded.bottom(Data_Time_Component.boundedSecond), Data_Bounded.bottom(Data_Time_Component.boundedMillisecond)), new Time(Data_Bounded.top(Data_Time_Component.boundedHour), Data_Bounded.top(Data_Time_Component.boundedMinute), Data_Bounded.top(Data_Time_Component.boundedSecond), Data_Bounded.top(Data_Time_Component.boundedMillisecond)));
var maxTime = timeToMillis(Data_Bounded.top(boundedTime));
var adjust = function (dictDuration) {
    return function (d) {
        return function (t) {
            var tLength = timeToMillis(t);
            var d$prime = Data_Time_Duration.fromDuration(dictDuration)(d);
            var wholeDays = Data_Time_Duration.Days($$Math.floor(Data_Newtype.unwrap(Data_Time_Duration.newtypeMilliseconds)(d$prime) / 8.64e7));
            var msAdjust = Data_Ring.sub(Data_Time_Duration.ringMilliseconds)(d$prime)(Data_Time_Duration.fromDuration(Data_Time_Duration.durationDays)(wholeDays));
            var msAdjusted = Data_Semiring.add(Data_Time_Duration.semiringMilliseconds)(tLength)(msAdjust);
            var wrap = (function () {
                var $134 = Data_Ord.greaterThan(Data_Time_Duration.ordMilliseconds)(msAdjusted)(maxTime);
                if ($134) {
                    return 1.0;
                };
                var $135 = Data_Ord.lessThan(Data_Time_Duration.ordMilliseconds)(msAdjusted)(Data_Ring.negate(Data_Time_Duration.ringMilliseconds)(maxTime));
                if ($135) {
                    return -1.0;
                };
                return 0.0;
            })();
            return new Data_Tuple.Tuple(Data_Semiring.add(Data_Time_Duration.semiringDays)(wholeDays)(wrap), millisToTime(Data_Ring.sub(Data_Time_Duration.ringMilliseconds)(msAdjusted)(8.64e7 * wrap)));
        };
    };
};
module.exports = {
    Time: Time, 
    adjust: adjust, 
    diff: diff, 
    hour: hour, 
    millisecond: millisecond, 
    minute: minute, 
    second: second, 
    setHour: setHour, 
    setMillisecond: setMillisecond, 
    setMinute: setMinute, 
    setSecond: setSecond, 
    eqTime: eqTime, 
    ordTime: ordTime, 
    genericTime: genericTime, 
    boundedTime: boundedTime, 
    showTime: showTime
};

},{"../Control.Apply":8,"../Data.Bounded":66,"../Data.Enum":76,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Function":88,"../Data.Functor":91,"../Data.Generic":93,"../Data.HeytingAlgebra":96,"../Data.Int":101,"../Data.Maybe":109,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Time.Component":140,"../Data.Time.Duration":141,"../Data.Tuple":145,"../Data.Unit":149,"../Math":166,"../Partial.Unsafe":168,"../Prelude":171}],143:[function(require,module,exports){
"use strict";

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function Cont(fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function consList(x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          var buildFrom = function (x, ys) {
            return apply(map(consList)(f(x)))(ys);
          };

          var go = function (acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last = xs[currentLen - 1];
              return new Cont(function () {
                return go(buildFrom(last, acc), currentLen - 1, xs);
              });
            }
          };

          return function (array) {
            var result = go(pure(emptyList), array.length, array);
            while (result instanceof Cont) {
              result = result.fn();
            }

            return map(listToArray)(result);
          };
        };
      };
    };
  };
}();

},{}],144:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Category = require("../Control.Category");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Maybe_Last = require("../Data.Maybe.Last");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Monoid_Conj = require("../Data.Monoid.Conj");
var Data_Monoid_Disj = require("../Data.Monoid.Disj");
var Data_Monoid_Dual = require("../Data.Monoid.Dual");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative");
var Prelude = require("../Prelude");
var StateL = function (x) {
    return x;
};
var StateR = function (x) {
    return x;
};
var Traversable = function (Foldable1, Functor0, sequence, traverse) {
    this.Foldable1 = Foldable1;
    this.Functor0 = Functor0;
    this.sequence = sequence;
    this.traverse = traverse;
};
var traverse = function (dict) {
    return dict.traverse;
};
var traversableMultiplicative = new Traversable(function () {
    return Data_Foldable.foldableMultiplicative;
}, function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Traversable line 87, column 3 - line 87, column 35: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Traversable line 85, column 3 - line 85, column 37: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableDual = new Traversable(function () {
    return Data_Foldable.foldableDual;
}, function () {
    return Data_Monoid_Dual.functorDual;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableDisj = new Traversable(function () {
    return Data_Foldable.foldableDisj;
}, function () {
    return Data_Monoid_Disj.functorDisj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(f(v));
        };
    };
});
var traversableConj = new Traversable(function () {
    return Data_Foldable.foldableConj;
}, function () {
    return Data_Monoid_Conj.functorConj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(f(v));
        };
    };
});
var traversableAdditive = new Traversable(function () {
    return Data_Foldable.foldableAdditive;
}, function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(f(v));
        };
    };
});
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return traverse(dictTraversable)(dictApplicative)(Control_Category.id(Control_Category.categoryFn));
    };
};
var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
}, function () {
    return Data_Functor.functorArray;
}, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
}, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative.Apply0()))(Data_Functor.map((dictApplicative.Apply0()).Functor0()))(Control_Applicative.pure(dictApplicative));
});
var sequence = function (dict) {
    return dict.sequence;
};
var traversableFirst = new Traversable(function () {
    return Data_Foldable.foldableFirst;
}, function () {
    return Data_Maybe_First.functorFirst;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traversableLast = new Traversable(function () {
    return Data_Foldable.foldableLast;
}, function () {
    return Data_Maybe_Last.functorLast;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Data_Functor.map(dictTraversable.Functor0())(f)(ta));
            };
        };
    };
};
var functorStateR = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var v = stateR(k)(s);
            return {
                accum: v.accum, 
                value: f(v.value)
            };
        };
    };
});
var functorStateL = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var v = stateL(k)(s);
            return {
                accum: v.accum, 
                value: f(v.value)
            };
        };
    };
});
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
var applyStateR = new Control_Apply.Apply(function () {
    return functorStateR;
}, function (f) {
    return function (x) {
        return function (s) {
            var v = stateR(x)(s);
            var v1 = stateR(f)(v.accum);
            return {
                accum: v1.accum, 
                value: v1.value(v.value)
            };
        };
    };
});
var applyStateL = new Control_Apply.Apply(function () {
    return functorStateL;
}, function (f) {
    return function (x) {
        return function (s) {
            var v = stateL(f)(s);
            var v1 = stateL(x)(v.accum);
            return {
                accum: v1.accum, 
                value: v.value(v1.value)
            };
        };
    };
});
var applicativeStateR = new Control_Applicative.Applicative(function () {
    return applyStateR;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateR(traverse(dictTraversable)(applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(a)(b);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var applicativeStateL = new Control_Applicative.Applicative(function () {
    return applyStateL;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateL(traverse(dictTraversable)(applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(b)(a);
                        return {
                            accum: b$prime, 
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
module.exports = {
    Traversable: Traversable, 
    "for": $$for, 
    mapAccumL: mapAccumL, 
    mapAccumR: mapAccumR, 
    scanl: scanl, 
    scanr: scanr, 
    sequence: sequence, 
    sequenceDefault: sequenceDefault, 
    traverse: traverse, 
    traverseDefault: traverseDefault, 
    traversableArray: traversableArray, 
    traversableMaybe: traversableMaybe, 
    traversableFirst: traversableFirst, 
    traversableLast: traversableLast, 
    traversableAdditive: traversableAdditive, 
    traversableDual: traversableDual, 
    traversableConj: traversableConj, 
    traversableDisj: traversableDisj, 
    traversableMultiplicative: traversableMultiplicative
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Category":13,"../Data.Foldable":83,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Maybe.First":107,"../Data.Maybe.Last":108,"../Data.Monoid.Additive":110,"../Data.Monoid.Conj":111,"../Data.Monoid.Disj":112,"../Data.Monoid.Dual":113,"../Data.Monoid.Multiplicative":115,"../Prelude":171,"./foreign":143}],145:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Control_Bind = require("../Control.Bind");
var Control_Comonad = require("../Control.Comonad");
var Control_Extend = require("../Control.Extend");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Bifoldable = require("../Data.Bifoldable");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Bitraversable = require("../Data.Bitraversable");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Functor_Invariant = require("../Data.Functor.Invariant");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(Tuple " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return new Data_Semiring.Semiring(function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.add(dictSemiring)(v.value0)(v1.value0), Data_Semiring.add(dictSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.mul(dictSemiring)(v.value0)(v1.value0), Data_Semiring.mul(dictSemiring1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_Semiring.one(dictSemiring), Data_Semiring.one(dictSemiring1)), new Tuple(Data_Semiring.zero(dictSemiring), Data_Semiring.zero(dictSemiring1)));
    };
};
var semigroupoidTuple = new Control_Semigroupoid.Semigroupoid(function (v) {
    return function (v1) {
        return new Tuple(v1.value0, v.value1);
    };
});
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Data_Semigroup.Semigroup(function (v) {
            return function (v1) {
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
            };
        });
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return new Data_Ring.Ring(function () {
            return semiringTuple(dictRing.Semiring0())(dictRing1.Semiring0());
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Ring.sub(dictRing)(v.value0)(v1.value0), Data_Ring.sub(dictRing1)(v.value1)(v1.value1));
            };
        });
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return new Data_Monoid.Monoid(function () {
            return semigroupTuple(dictMonoid.Semigroup0())(dictMonoid1.Semigroup0());
        }, new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)));
    };
};
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            return function ($261) {
                return Data_Newtype.unwrap(Data_Maybe_First.newtypeFirst)(Data_Foldable.foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                    var $143 = Data_Eq.eq(dictEq)(a)(v.value0);
                    if ($143) {
                        return new Data_Maybe.Just(v.value1);
                    };
                    return Data_Maybe.Nothing.value;
                })($261));
            };
        };
    };
};
var heytingAlgebraTuple = function (dictHeytingAlgebra) {
    return function (dictHeytingAlgebra1) {
        return new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.conj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.disj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra1)), function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.implies(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return new Tuple(Data_HeytingAlgebra.not(dictHeytingAlgebra)(v.value0), Data_HeytingAlgebra.not(dictHeytingAlgebra1)(v.value1));
        }, new Tuple(Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra1)));
    };
};
var functorTuple = new Data_Functor.Functor(function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v.value1));
    };
});
var invariantTuple = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorTuple));
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return new Control_Lazy.Lazy(function (f) {
            return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                return fst(f(Data_Unit.unit));
            }), Control_Lazy.defer(dictLazy1)(function (v) {
                return snd(f(Data_Unit.unit));
            }));
        });
    };
};
var foldableTuple = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v.value1)(z);
        };
    };
});
var traversableTuple = new Data_Traversable.Traversable(function () {
    return foldableTuple;
}, function () {
    return functorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var extendTuple = new Control_Extend.Extend(function () {
    return functorTuple;
}, function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v));
    };
});
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
            };
        });
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
            };
        });
    };
};
var eq1Tuple = function (dictEq) {
    return new Data_Eq.Eq1(function (dictEq1) {
        return Data_Eq.eq(eqTuple(dictEq)(dictEq1));
    });
};
var ord1Tuple = function (dictOrd) {
    return new Data_Ord.Ord1(function () {
        return eq1Tuple(dictOrd.Eq0());
    }, function (dictOrd1) {
        return Data_Ord.compare(ordTuple(dictOrd)(dictOrd1));
    });
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = new Control_Comonad.Comonad(function () {
    return extendTuple;
}, snd);
var commutativeRingTuple = function (dictCommutativeRing) {
    return function (dictCommutativeRing1) {
        return new Data_CommutativeRing.CommutativeRing(function () {
            return ringTuple(dictCommutativeRing.Ring0())(dictCommutativeRing1.Ring0());
        });
    };
};
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordTuple(dictBounded.Ord0())(dictBounded1.Ord0());
        }, new Tuple(Data_Bounded.bottom(dictBounded), Data_Bounded.bottom(dictBounded1)), new Tuple(Data_Bounded.top(dictBounded), Data_Bounded.top(dictBounded1)));
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return new Data_BooleanAlgebra.BooleanAlgebra(function () {
            return heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0())(dictBooleanAlgebra1.HeytingAlgebra0());
        });
    };
};
var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Tuple(f(v.value0), g(v.value1));
        };
    };
});
var bifoldableTuple = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(g(v.value1));
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return g(f(z)(v.value0))(v.value1);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return f(v.value0)(g(v.value1)(z));
            };
        };
    };
});
var bitraversableTuple = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableTuple;
}, function () {
    return bifunctorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create)(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create)(f(v.value0)))(g(v.value1));
            };
        };
    };
});
var biapplyTuple = new Control_Biapply.Biapply(function () {
    return bifunctorTuple;
}, function (v) {
    return function (v1) {
        return new Tuple(v.value0(v1.value0), v.value1(v1.value1));
    };
});
var biapplicativeTuple = new Control_Biapplicative.Biapplicative(function () {
    return biapplyTuple;
}, Tuple.create);
var applyTuple = function (dictSemigroup) {
    return new Control_Apply.Apply(function () {
        return functorTuple;
    }, function (v) {
        return function (v1) {
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
        };
    });
};
var bindTuple = function (dictSemigroup) {
    return new Control_Bind.Bind(function () {
        return applyTuple(dictSemigroup);
    }, function (v) {
        return function (f) {
            var v1 = f(v.value1);
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
        };
    });
};
var applicativeTuple = function (dictMonoid) {
    return new Control_Applicative.Applicative(function () {
        return applyTuple(dictMonoid.Semigroup0());
    }, Tuple.create(Data_Monoid.mempty(dictMonoid)));
};
var monadTuple = function (dictMonoid) {
    return new Control_Monad.Monad(function () {
        return applicativeTuple(dictMonoid);
    }, function () {
        return bindTuple(dictMonoid.Semigroup0());
    });
};
module.exports = {
    Tuple: Tuple, 
    curry: curry, 
    fst: fst, 
    lookup: lookup, 
    snd: snd, 
    swap: swap, 
    uncurry: uncurry, 
    showTuple: showTuple, 
    eqTuple: eqTuple, 
    eq1Tuple: eq1Tuple, 
    ordTuple: ordTuple, 
    ord1Tuple: ord1Tuple, 
    boundedTuple: boundedTuple, 
    semigroupoidTuple: semigroupoidTuple, 
    semigroupTuple: semigroupTuple, 
    monoidTuple: monoidTuple, 
    semiringTuple: semiringTuple, 
    ringTuple: ringTuple, 
    commutativeRingTuple: commutativeRingTuple, 
    heytingAlgebraTuple: heytingAlgebraTuple, 
    booleanAlgebraTuple: booleanAlgebraTuple, 
    functorTuple: functorTuple, 
    invariantTuple: invariantTuple, 
    bifunctorTuple: bifunctorTuple, 
    applyTuple: applyTuple, 
    biapplyTuple: biapplyTuple, 
    applicativeTuple: applicativeTuple, 
    biapplicativeTuple: biapplicativeTuple, 
    bindTuple: bindTuple, 
    monadTuple: monadTuple, 
    extendTuple: extendTuple, 
    comonadTuple: comonadTuple, 
    lazyTuple: lazyTuple, 
    foldableTuple: foldableTuple, 
    bifoldableTuple: bifoldableTuple, 
    traversableTuple: traversableTuple, 
    bitraversableTuple: bitraversableTuple
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Biapplicative":9,"../Control.Biapply":10,"../Control.Bind":12,"../Control.Comonad":14,"../Control.Extend":15,"../Control.Lazy":16,"../Control.Monad":38,"../Control.Semigroupoid":42,"../Data.Bifoldable":55,"../Data.Bifunctor":61,"../Data.Bitraversable":62,"../Data.BooleanAlgebra":64,"../Data.Bounded":66,"../Data.CommutativeRing":71,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Functor.Invariant":89,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Maybe.First":107,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Traversable":144,"../Data.Unit":149,"../Prelude":171}],146:[function(require,module,exports){
"use strict";

exports.unfoldrArrayImpl = function (isNothing) {
  return function (fromJust) {
    return function (fst) {
      return function (snd) {
        return function (f) {
          return function (b) {
            var result = [];
            var value = b;
            while (true) { // eslint-disable-line no-constant-condition
              var maybe = f(value);
              if (isNothing(maybe)) return result;
              var tuple = fromJust(maybe);
              result.push(fst(tuple));
              value = snd(tuple);
            }
          };
        };
      };
    };
  };
};

},{}],147:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Unfoldable = function (unfoldr) {
    this.unfoldr = unfoldr;
};
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableArray = new Unfoldable($foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
var replicate = function (dictUnfoldable) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $8 = i <= 0;
                if ($8) {
                    return Data_Maybe.Nothing.value;
                };
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1 | 0));
            };
            return unfoldr(dictUnfoldable)(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        return function (dictTraversable) {
            return function (n) {
                return function (m) {
                    return Data_Traversable.sequence(dictTraversable)(dictApplicative)(replicate(dictUnfoldable)(n)(m));
                };
            };
        };
    };
};
var singleton = function (dictUnfoldable) {
    return replicate(dictUnfoldable)(1);
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Unit.unit);
};
var fromMaybe = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function (b) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Maybe.Nothing.value))(b);
    });
};
module.exports = {
    Unfoldable: Unfoldable, 
    fromMaybe: fromMaybe, 
    none: none, 
    replicate: replicate, 
    replicateA: replicateA, 
    singleton: singleton, 
    unfoldr: unfoldr, 
    unfoldableArray: unfoldableArray
};

},{"../Data.Function":88,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ring":126,"../Data.Traversable":144,"../Data.Tuple":145,"../Data.Unit":149,"../Partial.Unsafe":168,"../Prelude":171,"./foreign":146}],148:[function(require,module,exports){
"use strict";

exports.unit = {};

},{}],149:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Data_Show = require("../Data.Show");
var showUnit = new Data_Show.Show(function (v) {
    return "unit";
});
module.exports = {
    showUnit: showUnit, 
    unit: $foreign.unit
};

},{"../Data.Show":132,"./foreign":148}],150:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_Show = require("../Data.Show");
var Void = function (x) {
    return x;
};
var absurd = function (a) {
    var spin = function (__copy_v) {
        var __tco_result;
        function __tco_loop(v) {
            __copy_v = v;
            return;
        };
        while (!false) {
            __tco_result = __tco_loop(__copy_v);
        };
        return __tco_result;
    };
    return spin(a);
};
var showVoid = new Data_Show.Show(absurd);
module.exports = {
    absurd: absurd, 
    showVoid: showVoid
};

},{"../Data.Show":132}],151:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var DOM = require("../DOM");
var Data_Maybe = require("../Data.Maybe");
var Flare = require("../Flare");
var Graphics_Canvas = require("../Graphics.Canvas");
var Graphics_Drawing = require("../Graphics.Drawing");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Signal_Channel = require("../Signal.Channel");
var runFlareDrawing = function (controls) {
    return function (canvasID) {
        return function (ui) {
            return function __do() {
                var v = Graphics_Canvas.getCanvasElementById(canvasID)();
                var canvas = Data_Maybe.fromJust()(v);
                var v1 = Graphics_Canvas.getContext2D(canvas)();
                var render$prime = function (drawing) {
                    return function __do() {
                        var v2 = Graphics_Canvas.getCanvasWidth(canvas)();
                        var v3 = Graphics_Canvas.getCanvasHeight(canvas)();
                        var v4 = Graphics_Canvas.clearRect(v1)({
                            x: 0.0, 
                            y: 0.0, 
                            w: v2, 
                            h: v3
                        })();
                        return Graphics_Drawing.render(v1)(drawing)();
                    };
                };
                return Flare.runFlareWith(controls)(render$prime)(ui)();
            };
        };
    };
};
module.exports = {
    runFlareDrawing: runFlareDrawing
};

},{"../Control.Bind":12,"../Control.Monad.Eff":27,"../DOM":48,"../Data.Maybe":109,"../Flare":153,"../Graphics.Canvas":157,"../Graphics.Drawing":159,"../Partial.Unsafe":168,"../Prelude":171,"../Signal.Channel":173}],152:[function(require,module,exports){
// module Flare
// jshint browser: true
// jshint node: true

"use strict";

exports.renderString = function(target) {
  return function(content) {
    return function() {
      document.getElementById(target).innerHTML = content;
    };
  };
};

exports.removeChildren = function(target) {
  return function() {
    var el = document.getElementById(target);

    // http://stackoverflow.com/a/3955238/704831
    while (el.firstChild) {
      el.removeChild(el.firstChild);
    }
  };
};

exports.appendComponent = function(target) {
  return function(el) {
    return function() {
      document.getElementById(target).appendChild(el);
    };
  };
};

// This function maintains a global state `window.flareID` to generate unique
// DOM element IDs. It is only called from functions with a DOM effect.
function getUniqueID() {
  if (window.flareID === undefined) {
    window.flareID = 0;
  }
  window.flareID = window.flareID + 1;
  return "flare-component-" + window.flareID.toString();
}

function createComponent(inputType, elementCallback, eventType, eventListener) {
  return function(label) {
    return function(initial) {
      return function(send) {
        return function() {
          var uid = getUniqueID();
          var el = elementCallback(initial);
          el.className = "flare-input-" + inputType;
          el.id = uid;

          var div = document.createElement("div");
          div.className = "flare-input";

          if (label !== "") {
            var labelEl = document.createElement("label");
            labelEl.htmlFor = uid;
            labelEl.appendChild(document.createTextNode(label));
            div.appendChild(labelEl);
          }

          div.appendChild(el);

          el.addEventListener(eventType, function(e) {
            var value = eventListener(e.target, initial);
            send(value)();
          });

          return div;
        };
      };
    };
  };
}

exports.cNumber = createComponent("number",
  function(initial) {
    var input = document.createElement("input");
    input.type = "number";
    input.step = "any";
    input.value = initial.toString();
    return input;
  },
  "input",
  function(t, initial) {
    var val = parseFloat(t.value);
    return (isNaN(val) ? initial : val);
  }
);

function clamp(min, max, initial, value) {
  if (isNaN(value)) {
    return initial;
  } else if (value < min) {
    return min;
  } else if (value > max) {
    return max;
  }
  return value;
}

exports.cNumberRange = function(type) {
  return function(min) {
    return function(max) {
      return function(step) {
        return createComponent("number-" + type,
          function(initial) {
            var input = document.createElement("input");
            input.type = type;
            input.min = min.toString();
            input.max = max.toString();
            input.step = step.toString();
            input.value = initial.toString();
            return input;
          },
          "input",
          function(t, initial) {
            return clamp(min, max, initial, parseFloat(t.value));
          }
        );
      };
    };
  };
};

exports.cIntRange = function(type) {
  return function(min) {
    return function(max) {
      return createComponent("int-" + type,
        function(initial) {
          var input = document.createElement("input");
          input.type = type;
          input.min = min.toString();
          input.max = max.toString();
          input.step = "1";
          input.value = initial.toString();
          return input;
        },
        "input",
        function(t, initial) {
          return clamp(min, max, initial, parseInt(t.value, 10));
        }
      );
    };
  };
};

exports.cString = createComponent("string",
  function(initial) {
    var input = document.createElement("input");
    input.type = "text";
    input.value = initial;
    return input;
  },
  "input",
  function(t, initial) {
    return t.value;
  }
);

exports.cStringPattern = function(pattern) {
  return createComponent("string-pattern",
    function(initial) {
      var input = document.createElement("input");
      input.type = "text";
      input.pattern = pattern;
      input.required = true;
      input.value = initial;
      return input;
    },
    "input",
    function(t, initial) {
      return t.value;
    }
  );
};

exports.cBoolean = createComponent("boolean",
  function(initial) {
    var input = document.createElement("input");
    input.type = "checkbox";
    input.checked = initial;
    return input;
  },
  "change",
  function(t, initial) {
    return t.checked;
  }
);

exports.cButton = function(vPressed) {
  return function(label) {
    return function(vDefault) {
      return function(send) {
        return function() {
          var div = document.createElement("div");
          div.className = "flare-input";

          var button = document.createElement("button");
          button.id = getUniqueID();
          button.className = "flare-input-button";
          button.appendChild(document.createTextNode(label));

          button.addEventListener('mousedown', function() {
            send(vPressed)();
          });
          button.addEventListener('mouseup', function() {
            send(vDefault)();
          });

          div.appendChild(button);
          return div;

        };
      };
    };
  };
};

exports.cSelect = function(xs) {
  return function(toString) {
    return createComponent("select",
      function(initial) {
        var select = document.createElement("select");

        var x, op;
        for (var i = 0; i < xs.length + 1; i++) {
          x = (i === 0) ? initial : xs[i - 1];
          op = document.createElement("option");
          op.appendChild(document.createTextNode(toString(x)));
          select.appendChild(op);
        }

        return select;
      },
      "change",
      function(t, initial) {
        var ix = t.selectedIndex;
        if (ix === 0) {
          return initial;
        }
        return xs[ix - 1];
      }
    );
  };
};

exports.cRadioGroup = function(xs) {
  return function(toString) {
    return function(label) {
      var uid = getUniqueID();
      return createComponent("radioGroup",
        function(initial) {
          var fieldset = document.createElement("fieldset");

          if (label !== "") {
            var legend = document.createElement("legend");
            legend.appendChild(document.createTextNode(label));
            fieldset.appendChild(legend);
          }

          var x, xid, op, labelEl;
          for (var i = 0; i < xs.length + 1; i++) {
            x = (i === 0) ? initial : xs[i - 1];
            xid = uid + "-" + i.toString();

            op = document.createElement("input");
            op.type = "radio";
            op.name = uid;
            op.id = xid;
            if (i === 0) {
              op.checked = "checked";
            }
            fieldset.appendChild(op);

            labelEl = document.createElement("label");
            labelEl.appendChild(document.createTextNode(toString(x)));
            labelEl.htmlFor = xid;
            fieldset.appendChild(labelEl);
          }

          return fieldset;
        },
        "change",
        function(t, initial) {
          var ix = parseInt(t.id.substr(uid.length + 1), 10);
          if (ix === 0) {
            return initial;
          }
          return xs[ix - 1];
        }
      )("");
    };
  };
};

exports.toFieldset = function(label) {
  return function(elements) {
    var fieldset = document.createElement("fieldset");

    if (label !== "") {
      var legend = document.createElement("legend");
      legend.appendChild(document.createTextNode(label));
      fieldset.appendChild(legend);
    }

    for (var i = 0; i < elements.length; i++) {
      fieldset.appendChild(elements[i]);
    }

    return fieldset;
  };
};

exports.cColor = createComponent("color",
  function(initial) {
    var input = document.createElement("input");
    input.type = "color";
    input.value = initial;
    return input;
  },
  "input",
  function(t, initial) {
    return t.value;
  }
);

function padNumber(num) {
  var str = num.toString();
  if (str.length == 1) {
    str = "0" + str;
  }
  return str;
}

exports.cDate = createComponent("date",
  function(initial) {
    var input = document.createElement("input");
    input.type = "date";
    input.value = initial.year.toString() + "-" +
                  padNumber(initial.month) + "-" +
                  padNumber(initial.day);
    return input;
  },
  "input",
  function(t, initial) {
    var parts = t.value.split("-");
    return { year: parseInt(parts[0], 10),
             month: parseInt(parts[1], 10),
             day: parseInt(parts[2])
           };
  }
);

exports.cTime = createComponent("time",
  function(initial) {
    var input = document.createElement("input");
    input.type = "time";
    input.value = padNumber(initial.hours.toString()) + ":" +
                  padNumber(initial.minutes.toString());
    return input;
  },
  "input",
  function(t, initial) {
    var parts = t.value.split(":");
    return { hours: parseInt(parts[0], 10),
             minutes: parseInt(parts[1], 10)
           };
  }
);

// vim: ts=2:sw=2

},{}],153:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Color = require("../Color");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Array = require("../Data.Array");
var Data_Bounded = require("../Data.Bounded");
var Data_Date = require("../Data.Date");
var Data_Date_Component = require("../Data.Date.Component");
var Data_Enum = require("../Data.Enum");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Maybe_First = require("../Data.Maybe.First");
var Data_Monoid = require("../Data.Monoid");
var Data_Newtype = require("../Data.Newtype");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Time = require("../Data.Time");
var Data_Time_Component = require("../Data.Time.Component");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_Channel = require("../Signal.Channel");
var Flare = (function () {
    function Flare(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Flare.create = function (value0) {
        return function (value1) {
            return new Flare(value0, value1);
        };
    };
    return Flare;
})();
var UI = function (x) {
    return x;
};
var wrap = function (sig) {
    return UI(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(new Flare([  ], sig)));
};
var setupFlare = function (v) {
    return function __do() {
        var v1 = v();
        return {
            components: v1.value0, 
            signal: v1.value1
        };
    };
};
var liftSF = function (f) {
    return function (v) {
        return function __do() {
            var v1 = v();
            return new Flare(v1.value0, f(v1.value1));
        };
    };
};
var lift = function (msig) {
    return UI(function __do() {
        var v = msig();
        return new Flare([  ], v);
    });
};
var functorFlare = new Data_Functor.Functor(function (f) {
    return function (v) {
        return new Flare(v.value0, Data_Functor.map(Signal.functorSignal)(f)(v.value1));
    };
});
var functorUI = new Data_Functor.Functor(function (f) {
    return function (v) {
        return UI(Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Functor.map(functorFlare)(f))(v));
    };
});
var foldp = function (f) {
    return function (x0) {
        return liftSF(Signal.foldp(f)(x0));
    };
};
var flareWith = function (controls) {
    return function (handler) {
        return function (v) {
            return function __do() {
                var v1 = v();
                $foreign.removeChildren(controls)();
                Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)($foreign.appendComponent(controls))(v1.value0)();
                return handler(v1.value1)();
            };
        };
    };
};
var runFlareWith = function (controls) {
    return function (handler) {
        return function (ui) {
            return flareWith(controls)(function ($103) {
                return Signal.runSignal(Data_Functor.map(Signal.functorSignal)(handler)($103));
            })(ui);
        };
    };
};
var runFlare = function (controls) {
    return function (target) {
        return runFlareWith(controls)($foreign.renderString(target));
    };
};
var runFlareShow = function (dictShow) {
    return function (controls) {
        return function (target) {
            return function (ui) {
                return runFlare(controls)(target)(Data_Functor.map(functorUI)(Data_Show.show(dictShow))(ui));
            };
        };
    };
};
var fieldset = function (label) {
    return function (v) {
        return UI(function __do() {
            var v1 = v();
            return new Flare([ $foreign.toFieldset(label)(v1.value0) ], v1.value1);
        });
    };
};
var createUI = function (createComp) {
    return function (label) {
        return function ($$default) {
            return UI(function __do() {
                var v = Signal_Channel.channel($$default)();
                var v1 = createComp(label)($$default)(Signal_Channel.send(v))();
                var signal = Signal_Channel.subscribe(v);
                return new Flare([ v1 ], signal);
            });
        };
    };
};
var date = function (label) {
    return function ($$default) {
        var toDate = function (v) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Enum.toEnum(Data_Date_Component.boundedEnumYear)(v.year))(function (v1) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(v.month))(function (v2) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(v.day))(function (v3) {
                        return Data_Date.exactDate(v1)(v2)(v3);
                    });
                });
            });
        };
        return Data_Functor.map(functorUI)(function ($104) {
            return Data_Maybe.fromMaybe($$default)(toDate($104));
        })(createUI($foreign.cDate)(label)({
            year: Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year($$default)), 
            month: Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month($$default)), 
            day: Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(Data_Date.day($$default))
        }));
    };
};
var date_ = date("");
var $$int = function (label) {
    return createUI($foreign.cIntRange("number")(Data_Bounded.bottom(Data_Bounded.boundedInt))(Data_Bounded.top(Data_Bounded.boundedInt)))(label);
};
var int_ = $$int("");
var intRange = function (label) {
    return function (min) {
        return function (max) {
            return function ($$default) {
                return createUI($foreign.cIntRange("number")(min)(max))(label)($$default);
            };
        };
    };
};
var intRange_ = intRange("");
var intSlider = function (label) {
    return function (min) {
        return function (max) {
            return function ($$default) {
                return createUI($foreign.cIntRange("range")(min)(max))(label)($$default);
            };
        };
    };
};
var intSlider_ = intSlider("");
var number = createUI($foreign.cNumber);
var number_ = number("");
var numberRange = function (label) {
    return function (min) {
        return function (max) {
            return function (step) {
                return function ($$default) {
                    return createUI($foreign.cNumberRange("number")(min)(max)(step))(label)($$default);
                };
            };
        };
    };
};
var numberRange_ = numberRange("");
var numberSlider = function (label) {
    return function (min) {
        return function (max) {
            return function (step) {
                return function ($$default) {
                    return createUI($foreign.cNumberRange("range")(min)(max)(step))(label)($$default);
                };
            };
        };
    };
};
var numberSlider_ = numberSlider("");
var radioGroup = function (dictFoldable) {
    return function (label) {
        return function (v) {
            return function (toString) {
                return createUI($foreign.cRadioGroup(Data_Array.fromFoldable(dictFoldable)(v.value1))(toString))(label)(v.value0);
            };
        };
    };
};
var radioGroup_ = function (dictFoldable) {
    return radioGroup(dictFoldable)("");
};
var select = function (dictFoldable) {
    return function (label) {
        return function (v) {
            return function (toString) {
                return createUI($foreign.cSelect(Data_Array.fromFoldable(dictFoldable)(v.value1))(toString))(label)(v.value0);
            };
        };
    };
};
var select_ = function (dictFoldable) {
    return select(dictFoldable)("");
};
var string = createUI($foreign.cString);
var string_ = string("");
var stringPattern = function (label) {
    return function (pattern) {
        return function ($$default) {
            return createUI($foreign.cStringPattern(pattern))(label)($$default);
        };
    };
};
var stringPattern_ = stringPattern("");
var time = function (label) {
    return function ($$default) {
        var toTime = function (v) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Time.Time.create)(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)(v.hours)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)(v.minutes)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)(0)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)(0));
        };
        return Data_Functor.map(functorUI)(function ($105) {
            return Data_Maybe.fromMaybe($$default)(toTime($105));
        })(createUI($foreign.cTime)(label)({
            hours: 0, 
            minutes: 30
        }));
    };
};
var time_ = time("");
var color = function (label) {
    return function ($$default) {
        return Data_Functor.map(functorUI)(function ($106) {
            return Data_Maybe.fromMaybe($$default)(Color.fromHexString($106));
        })(createUI($foreign.cColor)(label)(Color.toHexString($$default)));
    };
};
var color_ = color("");
var button = function (label) {
    return function (vDefault) {
        return function (vPressed) {
            return createUI($foreign.cButton(vPressed))(label)(vDefault);
        };
    };
};
var $$boolean = createUI($foreign.cBoolean);
var boolean_ = $$boolean("");
var optional = function (label) {
    return function (enabled) {
        return function (x) {
            var ret = function (v) {
                if (v) {
                    return new Data_Maybe.Just(x);
                };
                if (!v) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Flare line 247, column 9 - line 247, column 28: " + [ v.constructor.name ]);
            };
            return Data_Functor.map(functorUI)(ret)($$boolean(label)(enabled));
        };
    };
};
var optional_ = optional("");
var applyUIFlipped = function (v) {
    return function (v1) {
        return UI(function __do() {
            var v2 = v();
            var v3 = v1();
            return new Flare(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v2.value0)(v3.value0), Control_Apply.apply(Signal.applySignal)(v3.value1)(v2.value1));
        });
    };
};
var applyFlare = new Control_Apply.Apply(function () {
    return functorFlare;
}, function (v) {
    return function (v1) {
        return new Flare(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v.value0)(v1.value0), Control_Apply.apply(Signal.applySignal)(v.value1)(v1.value1));
    };
});
var applyUI = new Control_Apply.Apply(function () {
    return functorUI;
}, function (v) {
    return function (v1) {
        return UI(Control_Apply.lift2(Control_Monad_Eff.applyEff)(Control_Apply.apply(applyFlare))(v)(v1));
    };
});
var semigroupUI = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(Control_Apply.lift2(applyUI)(Data_Semigroup.append(dictSemigroup)));
};
var applicativeFlare = new Control_Applicative.Applicative(function () {
    return applyFlare;
}, function (x) {
    return new Flare([  ], Control_Applicative.pure(Signal.applicativeSignal)(x));
});
var applicativeUI = new Control_Applicative.Applicative(function () {
    return applyUI;
}, function (x) {
    return UI(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Control_Applicative.pure(applicativeFlare)(x)));
});
var buttons = function (dictTraversable) {
    return function (xs) {
        return function (toString) {
            var toButton = function (x) {
                return button(toString(x))(Data_Maybe.Nothing.value)(new Data_Maybe.Just(x));
            };
            return Data_Functor.map(functorUI)(function ($107) {
                return Data_Newtype.unwrap(Data_Maybe_First.newtypeFirst)(Data_Foldable.foldMap(dictTraversable.Foldable1())(Data_Maybe_First.monoidFirst)(Data_Maybe_First.First)($107));
            })(Data_Traversable.traverse(dictTraversable)(applicativeUI)(toButton)(xs));
        };
    };
};
var monoidUI = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupUI(dictMonoid.Semigroup0());
    }, Control_Applicative.pure(applicativeUI)(Data_Monoid.mempty(dictMonoid)));
};
module.exports = {
    applyUIFlipped: applyUIFlipped, 
    "boolean": $$boolean, 
    boolean_: boolean_, 
    button: button, 
    buttons: buttons, 
    color: color, 
    color_: color_, 
    date: date, 
    date_: date_, 
    fieldset: fieldset, 
    flareWith: flareWith, 
    foldp: foldp, 
    "int": $$int, 
    intRange: intRange, 
    intRange_: intRange_, 
    intSlider: intSlider, 
    intSlider_: intSlider_, 
    int_: int_, 
    lift: lift, 
    liftSF: liftSF, 
    number: number, 
    numberRange: numberRange, 
    numberRange_: numberRange_, 
    numberSlider: numberSlider, 
    numberSlider_: numberSlider_, 
    number_: number_, 
    optional: optional, 
    optional_: optional_, 
    radioGroup: radioGroup, 
    radioGroup_: radioGroup_, 
    runFlare: runFlare, 
    runFlareShow: runFlareShow, 
    runFlareWith: runFlareWith, 
    select: select, 
    select_: select_, 
    setupFlare: setupFlare, 
    string: string, 
    stringPattern: stringPattern, 
    stringPattern_: stringPattern_, 
    string_: string_, 
    time: time, 
    time_: time_, 
    wrap: wrap, 
    functorFlare: functorFlare, 
    applyFlare: applyFlare, 
    applicativeFlare: applicativeFlare, 
    functorUI: functorUI, 
    applyUI: applyUI, 
    applicativeUI: applicativeUI, 
    semigroupUI: semigroupUI, 
    monoidUI: monoidUI
};

},{"../Color":3,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Semigroupoid":42,"../DOM":48,"../DOM.Node.Types":45,"../Data.Array":53,"../Data.Bounded":66,"../Data.Date":74,"../Data.Date.Component":72,"../Data.Enum":76,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Maybe.First":107,"../Data.Monoid":116,"../Data.Newtype":118,"../Data.NonEmpty":119,"../Data.Semigroup":128,"../Data.Show":132,"../Data.Time":142,"../Data.Time.Component":140,"../Data.Traversable":144,"../Prelude":171,"../Signal":179,"../Signal.Channel":173,"./foreign":152}],154:[function(require,module,exports){
/* globals exports */
"use strict";

// module Global

exports.nan = NaN;

exports.isNaN = isNaN;

exports.infinity = Infinity;

exports.isFinite = isFinite;

exports.readInt = function (radix) {
  return function (n) {
    return parseInt(n, radix);
  };
};

exports.readFloat = parseFloat;

exports.decodeURI = decodeURI;
exports.encodeURI = encodeURI;
exports.decodeURIComponent = decodeURIComponent;
exports.encodeURIComponent = encodeURIComponent;

},{}],155:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    "decodeURI": $foreign["decodeURI"], 
    "decodeURIComponent": $foreign["decodeURIComponent"], 
    "encodeURI": $foreign["encodeURI"], 
    "encodeURIComponent": $foreign["encodeURIComponent"], 
    infinity: $foreign.infinity, 
    "isFinite": $foreign["isFinite"], 
    "isNaN": $foreign["isNaN"], 
    nan: $foreign.nan, 
    readFloat: $foreign.readFloat, 
    readInt: $foreign.readInt
};

},{"./foreign":154}],156:[function(require,module,exports){
/* global exports */
"use strict";

exports.canvasElementToImageSource = function(e) {
    return e;
};

exports.tryLoadImageImpl = function (src) {
  return function(e) {
        return function(f) {
            return function () {
                var img = new Image();
                img.src = src;
                img.addEventListener("load", function() {
                    f(img)();
                }, false);
                img.addEventListener("error", function(error) {
                    e();
                }, false);

                return {};
            }
        }
    };
};

exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
    return function() {
        var el = document.getElementById(id);
        if (el && el instanceof HTMLCanvasElement) {
            return Just(el);
        } else {
            return Nothing;
        }
    };
};

exports.getContext2D = function(c) {
    return function() {
        return c.getContext('2d');
    };
};

exports.getCanvasWidth = function(canvas) {
    return function() {
        return canvas.width;
    };
};

exports.getCanvasHeight = function(canvas) {
    return function() {
        return canvas.height;
    };
};

exports.setCanvasWidth = function(width) {
    return function(canvas) {
        return function() {
            canvas.width = width;
            return canvas;
        };
    };
};

exports.setCanvasHeight = function(height) {
    return function(canvas) {
        return function() {
            canvas.height = height;
            return canvas;
        };
    };
};

exports.canvasToDataURL = function(canvas) {
    return function() {
        return canvas.toDataURL();
    };
};

exports.setLineWidth = function(width) {
    return function(ctx) {
        return function() {
            ctx.lineWidth = width;
            return ctx;
        };
    };
};

exports.setFillStyle = function(style) {
    return function(ctx) {
        return function() {
            ctx.fillStyle = style;
            return ctx;
        };
    };
};

exports.setStrokeStyle = function(style) {
    return function(ctx) {
        return function() {
            ctx.strokeStyle = style;
            return ctx;
        };
    };
};

exports.setShadowColor = function(color) {
    return function(ctx) {
        return function() {
            ctx.shadowColor = color;
            return ctx;
        };
    };
};

exports.setShadowBlur = function(blur) {
    return function(ctx) {
        return function() {
            ctx.shadowBlur = blur;
            return ctx;
        };
    };
};

exports.setShadowOffsetX = function(offsetX) {
    return function(ctx) {
        return function() {
            ctx.shadowOffsetX = offsetX;
            return ctx;
        };
    };
};

exports.setShadowOffsetY = function(offsetY) {
    return function(ctx) {
        return function() {
            ctx.shadowOffsetY = offsetY;
            return ctx;
        };
    };
};

exports.setMiterLimit = function(limit) {
    return function(ctx) {
        return function() {
            ctx.miterLimit = limit;
            return ctx;
        };
    };
};

exports.setLineCapImpl = function(cap) {
    return function(ctx) {
        return function() {
            ctx.lineCap = cap;
            return ctx;
        };
    };
};

exports.setLineJoinImpl = function(join) {
    return function(ctx) {
        return function() {
            ctx.lineJoin = join;
            return ctx;
        };
    };
};

exports.setGlobalCompositeOperationImpl = function(ctx) {
    return function(op) {
        return function() {
            ctx.globalCompositeOperation = op;
            return ctx;
        };
    };
};

exports.setGlobalAlpha = function(ctx) {
    return function(alpha) {
        return function() {
            ctx.globalAlpha = alpha;
            return ctx;
        };
    };
};

exports.beginPath = function(ctx) {
    return function() {
        ctx.beginPath();
        return ctx;
    };
};

exports.stroke = function(ctx) {
    return function() {
        ctx.stroke();
        return ctx;
    };
};

exports.fill = function(ctx) {
    return function() {
        ctx.fill();
        return ctx;
    };
};

exports.clip = function(ctx) {
    return function() {
        ctx.clip();
        return ctx;
    };
};

exports.lineTo = function(ctx) {
    return function(x) {
        return function(y) {
            return function() {
                ctx.lineTo(x, y);
                return ctx;
            };
        };
    };
};

exports.moveTo = function(ctx) {
    return function(x) {
        return function(y) {
            return function() {
                ctx.moveTo(x, y);
                return ctx;
            };
        };
    };
};

exports.closePath = function(ctx) {
    return function() {
        ctx.closePath();
        return ctx;
    };
};

exports.arc = function(ctx) {
    return function(a) {
        return function() {
            ctx.arc(a.x, a.y, a.r, a.start, a.end);
            return ctx;
        };
    };
};

exports.rect = function(ctx) {
    return function(r) {
        return function() {
            ctx.rect(r.x, r.y, r.w, r.h);
            return ctx;
        };
    };
};

exports.fillRect = function(ctx) {
    return function(r) {
        return function() {
            ctx.fillRect(r.x, r.y, r.w, r.h);
            return ctx;
        };
    };
};

exports.strokeRect = function(ctx) {
    return function(r) {
        return function() {
            ctx.strokeRect(r.x, r.y, r.w, r.h);
            return ctx;
        };
    };
};

exports.scale = function(t) {
    return function(ctx) {
        return function() {
            ctx.scale(t.scaleX, t.scaleY);
            return ctx;
        };
    };
};

exports.rotate = function(angle) {
    return function(ctx) {
        return function() {
            ctx.rotate(angle);
            return ctx;
        };
    };
};

exports.translate = function(t) {
    return function(ctx) {
        return function() {
            ctx.translate(t.translateX, t.translateY);
            return ctx;
        };
    };
};

exports.transform = function(t) {
    return function(ctx) {
        return function() {
            ctx.transform(t.m11, t.m12, t.m21, t.m22, t.m31, t.m32);
            return ctx;
        };
    };
};

exports.clearRect = function(ctx) {
    return function(r) {
        return function() {
            ctx.clearRect(r.x, r.y, r.w, r.h);
            return ctx;
        };
    };
};

exports.textAlignImpl = function(ctx) {
    return function() {
        return ctx.textAlign;
    }
};

exports.setTextAlignImpl = function(ctx) {
    return function(textAlign) {
        return function() {
            ctx.textAlign = textAlign;
            return ctx;
        }
    }
};

exports.font = function(ctx) {
    return function() {
        return ctx.font;
    };
};

exports.setFont = function(fontspec) {
    return function(ctx) {
        return function() {
            ctx.font = fontspec;
            return ctx;
        };
    };
};

exports.fillText = function(ctx) {
    return function(text) {
        return function(x) {
            return function(y) {
                return function() {
                    ctx.fillText(text, x, y);
                    return ctx;
                };
            };
        };
    };
};

exports.strokeText = function(ctx) {
    return function(text) {
        return function(x) {
            return function(y) {
                return function() {
                    ctx.strokeText(text, x, y);
                    return ctx;
                };
            };
        };
    };
};

exports.measureText = function(ctx) {
    return function(text) {
        return function() {
            return ctx.measureText(text);
        };
    };
};

exports.save = function(ctx) {
    return function() {
        ctx.save();
        return ctx;
    };
};

exports.restore = function(ctx) {
    return function() {
        ctx.restore();
        return ctx;
    };
};

exports.imageDataWidth = function(image) {
    return image.width;
};

exports.imageDataHeight = function(image) {
    return image.height;
};

exports.imageDataBuffer = function(image) {
    return image.data;
};

exports.getImageData = function(ctx) {
    return function(x) {
        return function(y) {
            return function(w) {
                return function(h) {
                    return function() {
                        return ctx.getImageData(x, y, w, h);
                    };
                };
            };
        };
    };
};

exports.putImageDataFull = function(ctx) {
    return function(image_data) {
        return function(x) {
            return function(y) {
                return function(dx) {
                    return function(dy) {
                        return function(dw) {
                            return function(dh) {
                                return function() {
                                    ctx.putImageData(image_data, x, y, dx, dy, dw, dh);
                                    return ctx;
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};

exports.putImageData = function(ctx) {
    return function(image_data) {
        return function(x) {
            return function(y) {
                return function() {
                    ctx.putImageData(image_data, x, y);
                    return ctx;
                };
            };
        };
    };
};

exports.createImageData = function(ctx) {
    return function(sw) {
        return function(sh) {
            return function() {
                return ctx.createImageData(sw, sh);
            };
        };
    };
};

exports.createImageDataCopy = function(ctx) {
    return function(image_data) {
        return function() {
            return ctx.createImageData(image_data);
        };
    };
};

exports.drawImage = function(ctx) {
    return function(image_source) {
        return function(dx) {
            return function(dy) {
                return function() {
                    ctx.drawImage(image_source, dx, dy);
                    return ctx;
                };
            };
        };
    };
};

exports.drawImageScale = function(ctx) {
    return function(image_source) {
        return function(dx) {
            return function(dy) {
                return function(dWidth) {
                    return function(dHeight) {
                        return function() {
                            ctx.drawImage(image_source, dx, dy, dWidth, dHeight);
                            return ctx;
                        };
                    };
                };
            };
        };
    };
};

exports.drawImageFull = function(ctx) {
    return function(image_source) {
        return function(sx) {
            return function(sy) {
                return function(sWidth) {
                    return function(sHeight) {
                        return function(dx) {
                            return function(dy) {
                                return function(dWidth) {
                                    return function(dHeight) {
                                        return function() {
                                            ctx.drawImage(image_source, sx, sy, sWidth, sHeight, dx, dy, dWidth, dHeight);
                                            return ctx;
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};

exports.createPatternImpl = function(img) {
    return function(repeat) {
        return function(ctx) {
            return function() {
                return ctx.createPattern(img, repeat);
            };
        };
    };
};

exports.setPatternFillStyle = function(pattern) {
    return function(ctx) {
        return function() {
            ctx.fillStyle = pattern;
            return ctx;
        };
    };
};

exports.createLinearGradient = function(linearGradient) {
    return function(ctx) {
        return function() {
            return ctx.createLinearGradient(linearGradient.x0, linearGradient.y0, linearGradient.x1, linearGradient.y1);
        };
    };
};

exports.createRadialGradient = function(radialGradient) {
    return function(ctx) {
        return function() {
            return ctx.createRadialGradient(radialGradient.x0, radialGradient.y0, radialGradient.r0, radialGradient.x1, radialGradient.y1, radialGradient.r1);
        };
    };
};

exports.addColorStop = function(stop) {
    return function(color) {
        return function(gradient) {
            return function() {
                gradient.addColorStop(stop, color);
                return gradient;
            };
        };
    };
};

exports.setGradientFillStyle = function(gradient) {
    return function(ctx) {
        return function() {
            ctx.fillStyle = gradient;
            return ctx;
        };
    };
};

exports.quadraticCurveTo = function(qCurve) {
    return function(ctx) {
        return function() {
            ctx.quadraticCurveTo(qCurve.cpx, qCurve.cpy, qCurve.x, qCurve.y);
            return ctx;
        };
    };
};

exports.bezierCurveTo = function(bCurve) {
    return function(ctx) {
        return function() {
            ctx.bezierCurveTo(bCurve.cp1x, bCurve.cp1y, bCurve.cp2x, bCurve.cp2y, bCurve.x, bCurve.y);
            return ctx;
        };
    };
};

},{}],157:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Exception_Unsafe = require("../Control.Monad.Eff.Exception.Unsafe");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_ArrayBuffer_Types = require("../Data.ArrayBuffer.Types");
var Data_Function = require("../Data.Function");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var AlignLeft = (function () {
    function AlignLeft() {

    };
    AlignLeft.value = new AlignLeft();
    return AlignLeft;
})();
var AlignRight = (function () {
    function AlignRight() {

    };
    AlignRight.value = new AlignRight();
    return AlignRight;
})();
var AlignCenter = (function () {
    function AlignCenter() {

    };
    AlignCenter.value = new AlignCenter();
    return AlignCenter;
})();
var AlignStart = (function () {
    function AlignStart() {

    };
    AlignStart.value = new AlignStart();
    return AlignStart;
})();
var AlignEnd = (function () {
    function AlignEnd() {

    };
    AlignEnd.value = new AlignEnd();
    return AlignEnd;
})();
var Repeat = (function () {
    function Repeat() {

    };
    Repeat.value = new Repeat();
    return Repeat;
})();
var RepeatX = (function () {
    function RepeatX() {

    };
    RepeatX.value = new RepeatX();
    return RepeatX;
})();
var RepeatY = (function () {
    function RepeatY() {

    };
    RepeatY.value = new RepeatY();
    return RepeatY;
})();
var NoRepeat = (function () {
    function NoRepeat() {

    };
    NoRepeat.value = new NoRepeat();
    return NoRepeat;
})();
var BevelJoin = (function () {
    function BevelJoin() {

    };
    BevelJoin.value = new BevelJoin();
    return BevelJoin;
})();
var RoundJoin = (function () {
    function RoundJoin() {

    };
    RoundJoin.value = new RoundJoin();
    return RoundJoin;
})();
var MiterJoin = (function () {
    function MiterJoin() {

    };
    MiterJoin.value = new MiterJoin();
    return MiterJoin;
})();
var Round = (function () {
    function Round() {

    };
    Round.value = new Round();
    return Round;
})();
var Square = (function () {
    function Square() {

    };
    Square.value = new Square();
    return Square;
})();
var Butt = (function () {
    function Butt() {

    };
    Butt.value = new Butt();
    return Butt;
})();
var SourceOver = (function () {
    function SourceOver() {

    };
    SourceOver.value = new SourceOver();
    return SourceOver;
})();
var SourceIn = (function () {
    function SourceIn() {

    };
    SourceIn.value = new SourceIn();
    return SourceIn;
})();
var SourceOut = (function () {
    function SourceOut() {

    };
    SourceOut.value = new SourceOut();
    return SourceOut;
})();
var SourceAtop = (function () {
    function SourceAtop() {

    };
    SourceAtop.value = new SourceAtop();
    return SourceAtop;
})();
var DestinationOver = (function () {
    function DestinationOver() {

    };
    DestinationOver.value = new DestinationOver();
    return DestinationOver;
})();
var DestinationIn = (function () {
    function DestinationIn() {

    };
    DestinationIn.value = new DestinationIn();
    return DestinationIn;
})();
var DestinationOut = (function () {
    function DestinationOut() {

    };
    DestinationOut.value = new DestinationOut();
    return DestinationOut;
})();
var DestinationAtop = (function () {
    function DestinationAtop() {

    };
    DestinationAtop.value = new DestinationAtop();
    return DestinationAtop;
})();
var Lighter = (function () {
    function Lighter() {

    };
    Lighter.value = new Lighter();
    return Lighter;
})();
var Copy = (function () {
    function Copy() {

    };
    Copy.value = new Copy();
    return Copy;
})();
var Xor = (function () {
    function Xor() {

    };
    Xor.value = new Xor();
    return Xor;
})();
var Multiply = (function () {
    function Multiply() {

    };
    Multiply.value = new Multiply();
    return Multiply;
})();
var Screen = (function () {
    function Screen() {

    };
    Screen.value = new Screen();
    return Screen;
})();
var Overlay = (function () {
    function Overlay() {

    };
    Overlay.value = new Overlay();
    return Overlay;
})();
var Darken = (function () {
    function Darken() {

    };
    Darken.value = new Darken();
    return Darken;
})();
var Lighten = (function () {
    function Lighten() {

    };
    Lighten.value = new Lighten();
    return Lighten;
})();
var ColorDodge = (function () {
    function ColorDodge() {

    };
    ColorDodge.value = new ColorDodge();
    return ColorDodge;
})();
var ColorBurn = (function () {
    function ColorBurn() {

    };
    ColorBurn.value = new ColorBurn();
    return ColorBurn;
})();
var HardLight = (function () {
    function HardLight() {

    };
    HardLight.value = new HardLight();
    return HardLight;
})();
var SoftLight = (function () {
    function SoftLight() {

    };
    SoftLight.value = new SoftLight();
    return SoftLight;
})();
var Difference = (function () {
    function Difference() {

    };
    Difference.value = new Difference();
    return Difference;
})();
var Exclusion = (function () {
    function Exclusion() {

    };
    Exclusion.value = new Exclusion();
    return Exclusion;
})();
var Hue = (function () {
    function Hue() {

    };
    Hue.value = new Hue();
    return Hue;
})();
var Saturation = (function () {
    function Saturation() {

    };
    Saturation.value = new Saturation();
    return Saturation;
})();
var Color = (function () {
    function Color() {

    };
    Color.value = new Color();
    return Color;
})();
var Luminosity = (function () {
    function Luminosity() {

    };
    Luminosity.value = new Luminosity();
    return Luminosity;
})();
var withContext = function (ctx) {
    return function (action) {
        return function __do() {
            var v = $foreign.save(ctx)();
            var v1 = action();
            var v2 = $foreign.restore(ctx)();
            return v1;
        };
    };
};
var tryLoadImage = function (path) {
    return function (k) {
        return $foreign.tryLoadImageImpl(path)(k(Data_Maybe.Nothing.value))(function ($34) {
            return k(Data_Maybe.Just.create($34));
        });
    };
};
var textAlign = function (ctx) {
    var unsafeParseTextAlign = function (v) {
        if (v === "left") {
            return AlignLeft.value;
        };
        if (v === "right") {
            return AlignRight.value;
        };
        if (v === "center") {
            return AlignCenter.value;
        };
        if (v === "start") {
            return AlignStart.value;
        };
        if (v === "end") {
            return AlignEnd.value;
        };
        return Control_Monad_Eff_Exception_Unsafe.unsafeThrow("invalid TextAlign: " + v);
    };
    return Data_Functor.map(Control_Monad_Eff.functorEff)(unsafeParseTextAlign)($foreign.textAlignImpl(ctx));
};
var strokePath = function (ctx) {
    return function (path) {
        return function __do() {
            var v = $foreign.beginPath(ctx)();
            var v1 = path();
            var v2 = $foreign.stroke(ctx)();
            return v1;
        };
    };
};
var showTextAlign = new Data_Show.Show(function (v) {
    if (v instanceof AlignLeft) {
        return "AlignLeft";
    };
    if (v instanceof AlignRight) {
        return "AlignRight";
    };
    if (v instanceof AlignCenter) {
        return "AlignCenter";
    };
    if (v instanceof AlignStart) {
        return "AlignStart";
    };
    if (v instanceof AlignEnd) {
        return "AlignEnd";
    };
    throw new Error("Failed pattern match at Graphics.Canvas line 485, column 3 - line 486, column 3: " + [ v.constructor.name ]);
});
var showPatternRepeat = new Data_Show.Show(function (v) {
    if (v instanceof Repeat) {
        return "Repeat";
    };
    if (v instanceof RepeatX) {
        return "RepeatX";
    };
    if (v instanceof RepeatY) {
        return "RepeatY";
    };
    if (v instanceof NoRepeat) {
        return "NoRepeat";
    };
    throw new Error("Failed pattern match at Graphics.Canvas line 595, column 3 - line 596, column 3: " + [ v.constructor.name ]);
});
var showComposite = new Data_Show.Show(function (v) {
    if (v instanceof SourceOver) {
        return "SourceOver";
    };
    if (v instanceof SourceIn) {
        return "SourceIn";
    };
    if (v instanceof SourceOut) {
        return "SourceOut";
    };
    if (v instanceof SourceAtop) {
        return "SourceAtop";
    };
    if (v instanceof DestinationOver) {
        return "DestinationOver";
    };
    if (v instanceof DestinationIn) {
        return "DestinationIn";
    };
    if (v instanceof DestinationOut) {
        return "DestinationOut";
    };
    if (v instanceof DestinationAtop) {
        return "DestinationAtop";
    };
    if (v instanceof Lighter) {
        return "Lighter";
    };
    if (v instanceof Copy) {
        return "Copy";
    };
    if (v instanceof Xor) {
        return "Xor";
    };
    if (v instanceof Multiply) {
        return "Multiply";
    };
    if (v instanceof Screen) {
        return "Screen";
    };
    if (v instanceof Overlay) {
        return "Overlay";
    };
    if (v instanceof Darken) {
        return "Darken";
    };
    if (v instanceof Lighten) {
        return "Lighten";
    };
    if (v instanceof ColorDodge) {
        return "ColorDodge";
    };
    if (v instanceof ColorBurn) {
        return "ColorBurn";
    };
    if (v instanceof HardLight) {
        return "HardLight";
    };
    if (v instanceof SoftLight) {
        return "SoftLight";
    };
    if (v instanceof Difference) {
        return "Difference";
    };
    if (v instanceof Exclusion) {
        return "Exclusion";
    };
    if (v instanceof Hue) {
        return "Hue";
    };
    if (v instanceof Saturation) {
        return "Saturation";
    };
    if (v instanceof Color) {
        return "Color";
    };
    if (v instanceof Luminosity) {
        return "Luminosity";
    };
    throw new Error("Failed pattern match at Graphics.Canvas line 283, column 3 - line 284, column 3: " + [ v.constructor.name ]);
});
var setTextAlign = function (ctx) {
    return function (textalign) {
        var toString = function (v) {
            if (v instanceof AlignLeft) {
                return "left";
            };
            if (v instanceof AlignRight) {
                return "right";
            };
            if (v instanceof AlignCenter) {
                return "center";
            };
            if (v instanceof AlignStart) {
                return "start";
            };
            if (v instanceof AlignEnd) {
                return "end";
            };
            throw new Error("Failed pattern match at Graphics.Canvas line 513, column 5 - line 514, column 5: " + [ v.constructor.name ]);
        };
        return $foreign.setTextAlignImpl(ctx)(toString(textalign));
    };
};
var setLineJoin = function (v) {
    if (v instanceof BevelJoin) {
        return $foreign.setLineJoinImpl("bevel");
    };
    if (v instanceof RoundJoin) {
        return $foreign.setLineJoinImpl("round");
    };
    if (v instanceof MiterJoin) {
        return $foreign.setLineJoinImpl("miter");
    };
    throw new Error("Failed pattern match at Graphics.Canvas line 246, column 1 - line 247, column 1: " + [ v.constructor.name ]);
};
var setLineCap = function (v) {
    if (v instanceof Round) {
        return $foreign.setLineCapImpl("round");
    };
    if (v instanceof Square) {
        return $foreign.setLineCapImpl("square");
    };
    if (v instanceof Butt) {
        return $foreign.setLineCapImpl("butt");
    };
    throw new Error("Failed pattern match at Graphics.Canvas line 233, column 1 - line 234, column 1: " + [ v.constructor.name ]);
};
var setGlobalCompositeOperation = function (ctx) {
    return function (composite) {
        var toString = function (v) {
            if (v instanceof SourceOver) {
                return "source-over";
            };
            if (v instanceof SourceIn) {
                return "source-in";
            };
            if (v instanceof SourceOut) {
                return "source-out";
            };
            if (v instanceof SourceAtop) {
                return "source-atop";
            };
            if (v instanceof DestinationOver) {
                return "destination-over";
            };
            if (v instanceof DestinationIn) {
                return "destination-in";
            };
            if (v instanceof DestinationOut) {
                return "destination-out";
            };
            if (v instanceof DestinationAtop) {
                return "destination-atop";
            };
            if (v instanceof Lighter) {
                return "lighter";
            };
            if (v instanceof Copy) {
                return "copy";
            };
            if (v instanceof Xor) {
                return "xor";
            };
            if (v instanceof Multiply) {
                return "multiply";
            };
            if (v instanceof Screen) {
                return "screen";
            };
            if (v instanceof Overlay) {
                return "overlay";
            };
            if (v instanceof Darken) {
                return "darken";
            };
            if (v instanceof Lighten) {
                return "lighten";
            };
            if (v instanceof ColorDodge) {
                return "color-dodge";
            };
            if (v instanceof ColorBurn) {
                return "color-burn";
            };
            if (v instanceof HardLight) {
                return "hard-light";
            };
            if (v instanceof SoftLight) {
                return "soft-light";
            };
            if (v instanceof Difference) {
                return "difference";
            };
            if (v instanceof Exclusion) {
                return "exclusion";
            };
            if (v instanceof Hue) {
                return "hue";
            };
            if (v instanceof Saturation) {
                return "saturation";
            };
            if (v instanceof Color) {
                return "color";
            };
            if (v instanceof Luminosity) {
                return "luminosity";
            };
            throw new Error("Failed pattern match at Graphics.Canvas line 316, column 5 - line 317, column 5: " + [ v.constructor.name ]);
        };
        return $foreign.setGlobalCompositeOperationImpl(ctx)(toString(composite));
    };
};
var setCanvasDimensions = function (d) {
    return function (ce) {
        return Control_Bind.bind(Control_Monad_Eff.bindEff)($foreign.setCanvasHeight(d.height)(ce))($foreign.setCanvasWidth(d.width));
    };
};
var getCanvasElementById = function (elId) {
    return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
};
var getCanvasDimensions = function (ce) {
    return function __do() {
        var v = $foreign.getCanvasWidth(ce)();
        var v1 = $foreign.getCanvasHeight(ce)();
        return {
            width: v, 
            height: v1
        };
    };
};
var fillPath = function (ctx) {
    return function (path) {
        return function __do() {
            var v = $foreign.beginPath(ctx)();
            var v1 = path();
            var v2 = $foreign.fill(ctx)();
            return v1;
        };
    };
};
var createPattern = function (img) {
    return function (repeat) {
        var toString = function (v) {
            if (v instanceof Repeat) {
                return "repeat";
            };
            if (v instanceof RepeatX) {
                return "repeat-x";
            };
            if (v instanceof RepeatY) {
                return "repeat-y";
            };
            if (v instanceof NoRepeat) {
                return "no-repeat";
            };
            throw new Error("Failed pattern match at Graphics.Canvas line 606, column 5 - line 607, column 5: " + [ v.constructor.name ]);
        };
        return $foreign.createPatternImpl(img)(toString(repeat));
    };
};
module.exports = {
    SourceOver: SourceOver, 
    SourceIn: SourceIn, 
    SourceOut: SourceOut, 
    SourceAtop: SourceAtop, 
    DestinationOver: DestinationOver, 
    DestinationIn: DestinationIn, 
    DestinationOut: DestinationOut, 
    DestinationAtop: DestinationAtop, 
    Lighter: Lighter, 
    Copy: Copy, 
    Xor: Xor, 
    Multiply: Multiply, 
    Screen: Screen, 
    Overlay: Overlay, 
    Darken: Darken, 
    Lighten: Lighten, 
    ColorDodge: ColorDodge, 
    ColorBurn: ColorBurn, 
    HardLight: HardLight, 
    SoftLight: SoftLight, 
    Difference: Difference, 
    Exclusion: Exclusion, 
    Hue: Hue, 
    Saturation: Saturation, 
    Color: Color, 
    Luminosity: Luminosity, 
    Round: Round, 
    Square: Square, 
    Butt: Butt, 
    BevelJoin: BevelJoin, 
    RoundJoin: RoundJoin, 
    MiterJoin: MiterJoin, 
    Repeat: Repeat, 
    RepeatX: RepeatX, 
    RepeatY: RepeatY, 
    NoRepeat: NoRepeat, 
    AlignLeft: AlignLeft, 
    AlignRight: AlignRight, 
    AlignCenter: AlignCenter, 
    AlignStart: AlignStart, 
    AlignEnd: AlignEnd, 
    createPattern: createPattern, 
    fillPath: fillPath, 
    getCanvasDimensions: getCanvasDimensions, 
    getCanvasElementById: getCanvasElementById, 
    setCanvasDimensions: setCanvasDimensions, 
    setGlobalCompositeOperation: setGlobalCompositeOperation, 
    setLineCap: setLineCap, 
    setLineJoin: setLineJoin, 
    setTextAlign: setTextAlign, 
    strokePath: strokePath, 
    textAlign: textAlign, 
    tryLoadImage: tryLoadImage, 
    withContext: withContext, 
    showComposite: showComposite, 
    showTextAlign: showTextAlign, 
    showPatternRepeat: showPatternRepeat, 
    addColorStop: $foreign.addColorStop, 
    arc: $foreign.arc, 
    beginPath: $foreign.beginPath, 
    bezierCurveTo: $foreign.bezierCurveTo, 
    canvasElementToImageSource: $foreign.canvasElementToImageSource, 
    canvasToDataURL: $foreign.canvasToDataURL, 
    clearRect: $foreign.clearRect, 
    clip: $foreign.clip, 
    closePath: $foreign.closePath, 
    createImageData: $foreign.createImageData, 
    createImageDataCopy: $foreign.createImageDataCopy, 
    createLinearGradient: $foreign.createLinearGradient, 
    createRadialGradient: $foreign.createRadialGradient, 
    drawImage: $foreign.drawImage, 
    drawImageFull: $foreign.drawImageFull, 
    drawImageScale: $foreign.drawImageScale, 
    fill: $foreign.fill, 
    fillRect: $foreign.fillRect, 
    fillText: $foreign.fillText, 
    font: $foreign.font, 
    getCanvasHeight: $foreign.getCanvasHeight, 
    getCanvasWidth: $foreign.getCanvasWidth, 
    getContext2D: $foreign.getContext2D, 
    getImageData: $foreign.getImageData, 
    imageDataBuffer: $foreign.imageDataBuffer, 
    imageDataHeight: $foreign.imageDataHeight, 
    imageDataWidth: $foreign.imageDataWidth, 
    lineTo: $foreign.lineTo, 
    measureText: $foreign.measureText, 
    moveTo: $foreign.moveTo, 
    putImageData: $foreign.putImageData, 
    putImageDataFull: $foreign.putImageDataFull, 
    quadraticCurveTo: $foreign.quadraticCurveTo, 
    rect: $foreign.rect, 
    restore: $foreign.restore, 
    rotate: $foreign.rotate, 
    save: $foreign.save, 
    scale: $foreign.scale, 
    setCanvasHeight: $foreign.setCanvasHeight, 
    setCanvasWidth: $foreign.setCanvasWidth, 
    setFillStyle: $foreign.setFillStyle, 
    setFont: $foreign.setFont, 
    setGlobalAlpha: $foreign.setGlobalAlpha, 
    setGradientFillStyle: $foreign.setGradientFillStyle, 
    setLineWidth: $foreign.setLineWidth, 
    setMiterLimit: $foreign.setMiterLimit, 
    setPatternFillStyle: $foreign.setPatternFillStyle, 
    setShadowBlur: $foreign.setShadowBlur, 
    setShadowColor: $foreign.setShadowColor, 
    setShadowOffsetX: $foreign.setShadowOffsetX, 
    setShadowOffsetY: $foreign.setShadowOffsetY, 
    setStrokeStyle: $foreign.setStrokeStyle, 
    stroke: $foreign.stroke, 
    strokeRect: $foreign.strokeRect, 
    strokeText: $foreign.strokeText, 
    transform: $foreign.transform, 
    translate: $foreign.translate
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Monad.Eff.Exception.Unsafe":19,"../Control.Semigroupoid":42,"../Data.ArrayBuffer.Types":54,"../Data.Function":88,"../Data.Function.Uncurried":87,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171,"./foreign":156}],158:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Alt = require("../Control.Alt");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var FontOptions = function (x) {
    return x;
};
var FontFamily = function (x) {
    return x;
};
var Font = (function () {
    function Font(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Font.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Font(value0, value1, value2);
            };
        };
    };
    return Font;
})();
var smallCaps = {
    style: Data_Maybe.Nothing.value, 
    variant: new Data_Maybe.Just("small-caps"), 
    weight: Data_Maybe.Nothing.value
};
var serif = "serif";
var semigroupFontOptions = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return {
            style: Control_Alt.alt(Data_Maybe.altMaybe)(v.style)(v1.style), 
            variant: Control_Alt.alt(Data_Maybe.altMaybe)(v.variant)(v1.variant), 
            weight: Control_Alt.alt(Data_Maybe.altMaybe)(v.weight)(v1.weight)
        };
    };
});
var sansSerif = "sans-serif";
var optionsString = function (v) {
    return Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(" ")([ Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidString)(v.style), Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidString)(v.variant), Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidString)(v.weight) ]);
};
var oblique = {
    style: new Data_Maybe.Just("oblique"), 
    variant: Data_Maybe.Nothing.value, 
    weight: Data_Maybe.Nothing.value
};
var monospace = "monospace";
var monoidFontOptions = new Data_Monoid.Monoid(function () {
    return semigroupFontOptions;
}, {
    style: Data_Maybe.Nothing.value, 
    variant: Data_Maybe.Nothing.value, 
    weight: Data_Maybe.Nothing.value
});
var light = {
    style: Data_Maybe.Nothing.value, 
    variant: Data_Maybe.Nothing.value, 
    weight: new Data_Maybe.Just("lighter")
};
var italic = {
    style: new Data_Maybe.Just("italic"), 
    variant: Data_Maybe.Nothing.value, 
    weight: Data_Maybe.Nothing.value
};
var fontString = function (v) {
    return optionsString(v.value2) + (" " + (Data_Show.show(Data_Show.showInt)(v.value1) + ("px " + v.value0)));
};
var font = Font.create;
var fantasy = "fantasy";
var eqFontOptions = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(v.style)(v1.style) && (Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(v.variant)(v1.variant) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(v.weight)(v1.weight));
    };
});
var eqFontFamily = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var eqFont = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(eqFontFamily)(x.value0)(y.value0) && x.value1 === y.value1 && Data_Eq.eq(eqFontOptions)(x.value2)(y.value2);
    };
});
var customFont = FontFamily;
var cursive = "cursive";
var bolder = {
    style: Data_Maybe.Nothing.value, 
    variant: Data_Maybe.Nothing.value, 
    weight: new Data_Maybe.Just("bolder")
};
var bold = {
    style: Data_Maybe.Nothing.value, 
    variant: Data_Maybe.Nothing.value, 
    weight: new Data_Maybe.Just("bold")
};
module.exports = {
    bold: bold, 
    bolder: bolder, 
    cursive: cursive, 
    customFont: customFont, 
    fantasy: fantasy, 
    font: font, 
    fontString: fontString, 
    italic: italic, 
    light: light, 
    monospace: monospace, 
    oblique: oblique, 
    sansSerif: sansSerif, 
    serif: serif, 
    smallCaps: smallCaps, 
    eqFont: eqFont, 
    eqFontFamily: eqFontFamily, 
    eqFontOptions: eqFontOptions, 
    semigroupFontOptions: semigroupFontOptions, 
    monoidFontOptions: monoidFontOptions
};

},{"../Control.Alt":4,"../Data.Eq":78,"../Data.Foldable":83,"../Data.HeytingAlgebra":96,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Semigroup":128,"../Data.Show":132,"../Prelude":171}],159:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Color = require("../Color");
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Unit = require("../Data.Unit");
var Graphics_Canvas = require("../Graphics.Canvas");
var Graphics_Drawing_Font = require("../Graphics.Drawing.Font");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Shadow = function (x) {
    return x;
};
var Path = (function () {
    function Path(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Path.create = function (value0) {
        return function (value1) {
            return new Path(value0, value1);
        };
    };
    return Path;
})();
var Rectangle = (function () {
    function Rectangle(value0) {
        this.value0 = value0;
    };
    Rectangle.create = function (value0) {
        return new Rectangle(value0);
    };
    return Rectangle;
})();
var Arc = (function () {
    function Arc(value0) {
        this.value0 = value0;
    };
    Arc.create = function (value0) {
        return new Arc(value0);
    };
    return Arc;
})();
var Composite = (function () {
    function Composite(value0) {
        this.value0 = value0;
    };
    Composite.create = function (value0) {
        return new Composite(value0);
    };
    return Composite;
})();
var OutlineStyle = function (x) {
    return x;
};
var FillStyle = function (x) {
    return x;
};
var Fill = (function () {
    function Fill(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Fill.create = function (value0) {
        return function (value1) {
            return new Fill(value0, value1);
        };
    };
    return Fill;
})();
var Outline = (function () {
    function Outline(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Outline.create = function (value0) {
        return function (value1) {
            return new Outline(value0, value1);
        };
    };
    return Outline;
})();
var Text = (function () {
    function Text(value0, value1, value2, value3, value4) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
    };
    Text.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return new Text(value0, value1, value2, value3, value4);
                    };
                };
            };
        };
    };
    return Text;
})();
var Many = (function () {
    function Many(value0) {
        this.value0 = value0;
    };
    Many.create = function (value0) {
        return new Many(value0);
    };
    return Many;
})();
var Scale = (function () {
    function Scale(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Scale.create = function (value0) {
        return function (value1) {
            return new Scale(value0, value1);
        };
    };
    return Scale;
})();
var Translate = (function () {
    function Translate(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Translate.create = function (value0) {
        return function (value1) {
            return new Translate(value0, value1);
        };
    };
    return Translate;
})();
var Rotate = (function () {
    function Rotate(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Rotate.create = function (value0) {
        return function (value1) {
            return new Rotate(value0, value1);
        };
    };
    return Rotate;
})();
var Clipped = (function () {
    function Clipped(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Clipped.create = function (value0) {
        return function (value1) {
            return new Clipped(value0, value1);
        };
    };
    return Clipped;
})();
var WithShadow = (function () {
    function WithShadow(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    WithShadow.create = function (value0) {
        return function (value1) {
            return new WithShadow(value0, value1);
        };
    };
    return WithShadow;
})();
var translate = function (tx) {
    return function (ty) {
        return Translate.create({
            translateX: tx, 
            translateY: ty
        });
    };
};
var text = Text.create;
var shadowOffset = function (x) {
    return function (y) {
        return {
            color: Data_Maybe.Nothing.value, 
            blur: Data_Maybe.Nothing.value, 
            offset: new Data_Maybe.Just({
                x: x, 
                y: y
            })
        };
    };
};
var shadowColor = function (c) {
    return {
        color: new Data_Maybe.Just(c), 
        blur: Data_Maybe.Nothing.value, 
        offset: Data_Maybe.Nothing.value
    };
};
var shadowBlur = function (b) {
    return {
        color: Data_Maybe.Nothing.value, 
        blur: new Data_Maybe.Just(b), 
        offset: Data_Maybe.Nothing.value
    };
};
var shadow = WithShadow.create;
var semigroupShape = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Composite) {
            return new Composite(Data_Semigroup.append(Data_List_Types.semigroupList)(v.value0)(Data_List.singleton(v1)));
        };
        if (v1 instanceof Composite) {
            return new Composite(new Data_List_Types.Cons(v, v1.value0));
        };
        return new Composite(new Data_List_Types.Cons(v, new Data_List_Types.Cons(v1, Data_List_Types.Nil.value)));
    };
});
var semigroupShadow = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return {
            color: Control_Alt.alt(Data_Maybe.altMaybe)(v.color)(v1.color), 
            blur: Control_Alt.alt(Data_Maybe.altMaybe)(v.blur)(v1.blur), 
            offset: Control_Alt.alt(Data_Maybe.altMaybe)(v.offset)(v1.offset)
        };
    };
});
var semigroupOutlineStyle = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return {
            color: Control_Alt.alt(Data_Maybe.altMaybe)(v.color)(v1.color), 
            lineWidth: Control_Alt.alt(Data_Maybe.altMaybe)(v.lineWidth)(v1.lineWidth)
        };
    };
});
var semigroupFillStyle = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return {
            color: Control_Alt.alt(Data_Maybe.altMaybe)(v.color)(v1.color)
        };
    };
});
var semigroupDrawing = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Many) {
            return new Many(Data_Semigroup.append(Data_List_Types.semigroupList)(v.value0)(Data_List.singleton(v1)));
        };
        if (v1 instanceof Many) {
            return new Many(new Data_List_Types.Cons(v, v1.value0));
        };
        return new Many(new Data_List_Types.Cons(v, new Data_List_Types.Cons(v1, Data_List_Types.Nil.value)));
    };
});
var scale = function (sx) {
    return function (sy) {
        return Scale.create({
            scaleX: sx, 
            scaleY: sy
        });
    };
};
var rotate = Rotate.create;
var render = function (ctx) {
    var renderShape = function (v) {
        if (v instanceof Path && v.value1 instanceof Data_List_Types.Nil) {
            return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
        };
        if (v instanceof Path && v.value1 instanceof Data_List_Types.Cons) {
            return function __do() {
                var v1 = Graphics_Canvas.moveTo(ctx)(v.value1.value0.x)(v.value1.value0.y)();
                Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_List_Types.foldableList)(v.value1.value1)(function (pt) {
                    return Graphics_Canvas.lineTo(ctx)(pt.x)(pt.y);
                })();
                return Control_Applicative.when(Control_Monad_Eff.applicativeEff)(v.value0)(Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.closePath(ctx)))();
            };
        };
        if (v instanceof Rectangle) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.rect(ctx)(v.value0));
        };
        if (v instanceof Arc) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.arc(ctx)(v.value0));
        };
        if (v instanceof Composite) {
            return Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_List_Types.foldableList)(v.value0)(renderShape);
        };
        throw new Error("Failed pattern match at Graphics.Drawing line 290, column 3 - line 290, column 39: " + [ v.constructor.name ]);
    };
    var applyShadow = function (v) {
        return function __do() {
            Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.color)(function (color) {
                return Graphics_Canvas.setShadowColor(Color.cssStringHSLA(color))(ctx);
            })();
            Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.blur)(function (blur) {
                return Graphics_Canvas.setShadowBlur(blur)(ctx);
            })();
            return Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.offset)(function (offset) {
                return function __do() {
                    var v1 = Graphics_Canvas.setShadowOffsetX(offset.x)(ctx)();
                    return Graphics_Canvas.setShadowOffsetY(offset.y)(ctx)();
                };
            })();
        };
    };
    var applyOutlineStyle = function (v) {
        return function __do() {
            Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.color)(function (color) {
                return Graphics_Canvas.setStrokeStyle(Color.cssStringHSLA(color))(ctx);
            })();
            return Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.lineWidth)(function (width) {
                return Graphics_Canvas.setLineWidth(width)(ctx);
            })();
        };
    };
    var applyFillStyle = function (v) {
        return Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableMaybe)(v.color)(function (color) {
            return Graphics_Canvas.setFillStyle(Color.cssStringHSLA(color))(ctx);
        });
    };
    var go = function (v) {
        if (v instanceof Fill) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                applyFillStyle(v.value1)();
                return Graphics_Canvas.fillPath(ctx)(renderShape(v.value0))();
            }));
        };
        if (v instanceof Outline) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                applyOutlineStyle(v.value1)();
                return Graphics_Canvas.strokePath(ctx)(renderShape(v.value0))();
            }));
        };
        if (v instanceof Many) {
            return Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_List_Types.foldableList)(v.value0)(go);
        };
        if (v instanceof Scale) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                var v1 = Graphics_Canvas.scale(v.value0)(ctx)();
                return go(v.value1)();
            }));
        };
        if (v instanceof Translate) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                var v1 = Graphics_Canvas.translate(v.value0)(ctx)();
                return go(v.value1)();
            }));
        };
        if (v instanceof Rotate) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                var v1 = Graphics_Canvas.rotate(v.value0)(ctx)();
                return go(v.value1)();
            }));
        };
        if (v instanceof Clipped) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                renderShape(v.value0)();
                var v1 = Graphics_Canvas.clip(ctx)();
                return go(v.value1)();
            }));
        };
        if (v instanceof WithShadow) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                applyShadow(v.value0)();
                return go(v.value1)();
            }));
        };
        if (v instanceof Text) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.withContext(ctx)(function __do() {
                var v1 = Graphics_Canvas.setFont(Graphics_Drawing_Font.fontString(v.value0))(ctx)();
                applyFillStyle(v.value3)();
                return Graphics_Canvas.fillText(ctx)(v.value4)(v.value1)(v.value2)();
            }));
        };
        throw new Error("Failed pattern match at Graphics.Drawing line 240, column 14 - line 297, column 40: " + [ v.constructor.name ]);
    };
    return go;
};
var rectangle = function (x) {
    return function (y) {
        return function (w) {
            return function (h) {
                return new Rectangle({
                    x: x, 
                    y: y, 
                    w: w, 
                    h: h
                });
            };
        };
    };
};
var path = function (dictFoldable) {
    return function ($203) {
        return Path.create(false)(Data_List.fromFoldable(dictFoldable)($203));
    };
};
var outlined = Data_Function.flip(Outline.create);
var outlineColor = function (c) {
    return {
        color: new Data_Maybe.Just(c), 
        lineWidth: Data_Maybe.Nothing.value
    };
};
var monoidShape = new Data_Monoid.Monoid(function () {
    return semigroupShape;
}, new Composite(Data_Monoid.mempty(Data_List_Types.monoidList)));
var monoidShadow = new Data_Monoid.Monoid(function () {
    return semigroupShadow;
}, {
    color: Data_Maybe.Nothing.value, 
    blur: Data_Maybe.Nothing.value, 
    offset: Data_Maybe.Nothing.value
});
var monoidOutlineStyle = new Data_Monoid.Monoid(function () {
    return semigroupOutlineStyle;
}, {
    color: Data_Maybe.Nothing.value, 
    lineWidth: Data_Maybe.Nothing.value
});
var monoidFillStyle = new Data_Monoid.Monoid(function () {
    return semigroupFillStyle;
}, {
    color: Data_Maybe.Nothing.value
});
var monoidDrawing = new Data_Monoid.Monoid(function () {
    return semigroupDrawing;
}, new Many(Data_Monoid.mempty(Data_List_Types.monoidList)));
var lineWidth = function (c) {
    return {
        color: Data_Maybe.Nothing.value, 
        lineWidth: new Data_Maybe.Just(c)
    };
};
var filled = Data_Function.flip(Fill.create);
var fillColor = function (c) {
    return {
        color: new Data_Maybe.Just(c)
    };
};
var everywhere = function (f) {
    var go = function (v) {
        if (v instanceof Many) {
            return f(new Many(Data_Functor.map(Data_List_Types.functorList)(go)(v.value0)));
        };
        if (v instanceof Scale) {
            return f(new Scale(v.value0, go(v.value1)));
        };
        if (v instanceof Translate) {
            return f(new Translate(v.value0, go(v.value1)));
        };
        if (v instanceof Rotate) {
            return f(new Rotate(v.value0, go(v.value1)));
        };
        if (v instanceof Clipped) {
            return f(new Clipped(v.value0, go(v.value1)));
        };
        if (v instanceof WithShadow) {
            return f(new WithShadow(v.value0, go(v.value1)));
        };
        return f(v);
    };
    return go;
};
var eqShape = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof Path && v1 instanceof Path) {
            return v.value0 === v1.value0 && (Data_Eq.eq(Data_List_Types.eqList(Data_Eq.eqNumber))(Data_Functor.map(Data_List_Types.functorList)(function (v2) {
                return v2.x;
            })(v.value1))(Data_Functor.map(Data_List_Types.functorList)(function (v2) {
                return v2.x;
            })(v1.value1)) && Data_Eq.eq(Data_List_Types.eqList(Data_Eq.eqNumber))(Data_Functor.map(Data_List_Types.functorList)(function (v2) {
                return v2.y;
            })(v.value1))(Data_Functor.map(Data_List_Types.functorList)(function (v2) {
                return v2.y;
            })(v1.value1)));
        };
        if (v instanceof Rectangle && v1 instanceof Rectangle) {
            return v.value0.x === v1.value0.x && (v.value0.y === v1.value0.y && (v.value0.w === v1.value0.w && v.value0.h === v1.value0.h));
        };
        if (v instanceof Arc && v1 instanceof Arc) {
            return v.value0.x === v1.value0.x && (v.value0.y === v1.value0.y && (v.value0.start === v1.value0.start && (v.value0.end === v1.value0.end && v.value0.r === v1.value0.r)));
        };
        if (v instanceof Composite && v1 instanceof Composite) {
            return Data_Eq.eq(Data_List_Types.eqList(eqShape))(v.value0)(v1.value0);
        };
        return false;
    };
});
var eqShadow = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Color.eqColor))(v.color)(v1.color) && (Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqNumber))(v.blur)(v1.blur) && Data_Maybe.maybe(Data_Maybe.isNothing(v1.offset))(function (o) {
            return Data_Maybe.maybe(false)(function (o$prime) {
                return o.x === o$prime.x && o.y === o$prime.y;
            })(v1.offset);
        })(v.offset));
    };
});
var eqOutlineStyle = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Color.eqColor))(x.color)(y.color) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqNumber))(x.lineWidth)(y.lineWidth);
    };
});
var eqFillStyle = new Data_Eq.Eq(function (x) {
    return function (y) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Color.eqColor))(x.color)(y.color);
    };
});
var eqDrawing = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Fill && y instanceof Fill) {
            return Data_Eq.eq(eqShape)(x.value0)(y.value0) && Data_Eq.eq(eqFillStyle)(x.value1)(y.value1);
        };
        if (x instanceof Outline && y instanceof Outline) {
            return Data_Eq.eq(eqShape)(x.value0)(y.value0) && Data_Eq.eq(eqOutlineStyle)(x.value1)(y.value1);
        };
        if (x instanceof Text && y instanceof Text) {
            return Data_Eq.eq(Graphics_Drawing_Font.eqFont)(x.value0)(y.value0) && x.value1 === y.value1 && x.value2 === y.value2 && Data_Eq.eq(eqFillStyle)(x.value3)(y.value3) && x.value4 === y.value4;
        };
        if (x instanceof Many && y instanceof Many) {
            return Data_Eq.eq(Data_List_Types.eqList(eqDrawing))(x.value0)(y.value0);
        };
        if (x instanceof Scale && y instanceof Scale) {
            return x.value0.scaleX === y.value0.scaleX && x.value0.scaleY === y.value0.scaleY && Data_Eq.eq(eqDrawing)(x.value1)(y.value1);
        };
        if (x instanceof Translate && y instanceof Translate) {
            return x.value0.translateX === y.value0.translateX && x.value0.translateY === y.value0.translateY && Data_Eq.eq(eqDrawing)(x.value1)(y.value1);
        };
        if (x instanceof Rotate && y instanceof Rotate) {
            return x.value0 === y.value0 && Data_Eq.eq(eqDrawing)(x.value1)(y.value1);
        };
        if (x instanceof Clipped && y instanceof Clipped) {
            return Data_Eq.eq(eqShape)(x.value0)(y.value0) && Data_Eq.eq(eqDrawing)(x.value1)(y.value1);
        };
        if (x instanceof WithShadow && y instanceof WithShadow) {
            return Data_Eq.eq(eqShadow)(x.value0)(y.value0) && Data_Eq.eq(eqDrawing)(x.value1)(y.value1);
        };
        return false;
    };
});
var closed = function (dictFoldable) {
    return function ($204) {
        return Path.create(true)(Data_List.fromFoldable(dictFoldable)($204));
    };
};
var clipped = Clipped.create;
var arc = function (x) {
    return function (y) {
        return function (start) {
            return function (end) {
                return function (r) {
                    return new Arc({
                        x: x, 
                        y: y, 
                        start: start, 
                        end: end, 
                        r: r
                    });
                };
            };
        };
    };
};
var circle = function (x) {
    return function (y) {
        return arc(x)(y)(0.0)($$Math.pi * 2.0);
    };
};
module.exports = {
    arc: arc, 
    circle: circle, 
    clipped: clipped, 
    closed: closed, 
    everywhere: everywhere, 
    fillColor: fillColor, 
    filled: filled, 
    lineWidth: lineWidth, 
    outlineColor: outlineColor, 
    outlined: outlined, 
    path: path, 
    rectangle: rectangle, 
    render: render, 
    rotate: rotate, 
    scale: scale, 
    shadow: shadow, 
    shadowBlur: shadowBlur, 
    shadowColor: shadowColor, 
    shadowOffset: shadowOffset, 
    text: text, 
    translate: translate, 
    semigroupShape: semigroupShape, 
    monoidShape: monoidShape, 
    eqShape: eqShape, 
    semigroupFillStyle: semigroupFillStyle, 
    monoidFillStyle: monoidFillStyle, 
    eqFillStyle: eqFillStyle, 
    semigroupOutlineStyle: semigroupOutlineStyle, 
    monoidOutlineStyle: monoidOutlineStyle, 
    eqOutlineStyle: eqOutlineStyle, 
    eqShadow: eqShadow, 
    semigroupShadow: semigroupShadow, 
    monoidShadow: monoidShadow, 
    semigroupDrawing: semigroupDrawing, 
    monoidDrawing: monoidDrawing, 
    eqDrawing: eqDrawing
};

},{"../Color":3,"../Control.Alt":4,"../Control.Applicative":6,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Semigroupoid":42,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.List":105,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Unit":149,"../Graphics.Canvas":157,"../Graphics.Drawing.Font":158,"../Math":166,"../Prelude":171}],160:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Graph = require("../Data.Graph");
var Data_List = require("../Data.List");
var Data_List_ThreeOrMore = require("../Data.List.ThreeOrMore");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Graphics_Isometric = require("../Graphics.Isometric");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Vertex = (function () {
    function Vertex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Vertex.create = function (value0) {
        return function (value1) {
            return new Vertex(value0, value1);
        };
    };
    return Vertex;
})();
var toScene = function (vertices) {
    var dropIndex = function (v) {
        return v.value1;
    };
    return Data_Foldable.foldMap(Data_List_Types.foldableList)(Graphics_Isometric.monoidScene)(dropIndex)(vertices);
};
var flatten = function (v) {
    if (v instanceof Graphics_Isometric.Many) {
        return Control_Bind.bind(Data_List_Types.bindList)(v.value0)(flatten);
    };
    return Data_List.singleton(v);
};
var eqVertex = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v.value0 === v1.value0;
    };
});
var ordVertex = new Data_Ord.Ord(function () {
    return eqVertex;
}, function (v) {
    return function (v1) {
        return Data_Ord.compare(Data_Ord.ordInt)(v.value0)(v1.value0);
    };
});
var bounds = function (shape) {
    var min$prime = function ($45) {
        return Data_Maybe.fromJust()(Data_Foldable.minimum(Data_Ord.ordNumber)(Data_List_Types.foldableList)($45));
    };
    var max$prime = function ($46) {
        return Data_Maybe.fromJust()(Data_Foldable.maximum(Data_Ord.ordNumber)(Data_List_Types.foldableList)($46));
    };
    var coords = Data_List.concat(Data_Functor.map(Data_List_Types.functorList)(Data_List_ThreeOrMore.toList)(shape));
    var cZ = Data_Functor.map(Data_List_Types.functorList)(function (v) {
        return v.z;
    })(coords);
    var cY = Data_Functor.map(Data_List_Types.functorList)(function (v) {
        return v.y;
    })(coords);
    var cX = Data_Functor.map(Data_List_Types.functorList)(function (v) {
        return v.x;
    })(coords);
    return {
        minX: min$prime(cX), 
        maxX: max$prime(cX), 
        minY: min$prime(cY), 
        maxY: max$prime(cY), 
        minZ: min$prime(cZ), 
        maxZ: max$prime(cZ)
    };
};
var isBehind = function (dictPartial) {
    return function (v) {
        return function (v1) {
            var __unused = function (dictPartial1) {
                return function ($dollar14) {
                    return $dollar14;
                };
            };
            return __unused(dictPartial)((function () {
                if (v.value1 instanceof Graphics_Isometric.Fill && v1.value1 instanceof Graphics_Isometric.Fill) {
                    var b2 = bounds(v1.value1.value1);
                    var b1 = bounds(v.value1.value1);
                    var decide = (function () {
                        if (b1.maxX <= b2.minX) {
                            return true;
                        };
                        if (b2.maxX <= b1.minX) {
                            return false;
                        };
                        if (b1.maxY <= b2.minY) {
                            return true;
                        };
                        if (b2.maxY <= b1.minY) {
                            return false;
                        };
                        if (b1.maxZ <= b2.minZ) {
                            return true;
                        };
                        if (b2.maxZ <= b1.minZ) {
                            return false;
                        };
                        if (Data_Boolean.otherwise) {
                            return true;
                        };
                        throw new Error("Failed pattern match at Graphics.Isometric.DepthSort line 54, column 58 - line 65, column 25: " + [  ]);
                    })();
                    return decide;
                };
                throw new Error("Failed pattern match at Graphics.Isometric.DepthSort line 54, column 1 - line 65, column 25: " + [ v.constructor.name, v1.constructor.name ]);
            })());
        };
    };
};
var toGraph = function (scene) {
    var addKey = function (scene1) {
        return function (key) {
            return new Vertex(key, scene1);
        };
    };
    var addKeys = function (list) {
        return Data_List.zipWith(addKey)(list)(Data_List.range(0)(Data_List.length(list) - 1 | 0));
    };
    var vertices = addKeys(flatten(scene));
    var edges = function (i) {
        return Data_List.filter(function (j) {
            return isBehind()(i)(j);
        })(vertices);
    };
    return Data_Graph.unfoldGraph(ordVertex)(Data_List_Types.functorList)(Data_List_Types.foldableList)(Data_List_Types.foldableList)(vertices)(Control_Category.id(Control_Category.categoryFn))(edges);
};
var depthSort = function ($47) {
    return toScene(Data_Graph.topologicalSort(ordVertex)(toGraph($47)));
};
module.exports = {
    depthSort: depthSort
};

},{"../Control.Bind":12,"../Control.Category":13,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.Eq":78,"../Data.Foldable":83,"../Data.Functor":91,"../Data.Graph":94,"../Data.List":105,"../Data.List.ThreeOrMore":103,"../Data.List.Types":104,"../Data.Maybe":109,"../Data.Ord":123,"../Data.Ring":126,"../Graphics.Isometric":163,"../Partial.Unsafe":168,"../Prelude":171}],161:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Graphics_Drawing = require("../Graphics.Drawing");
var Graphics_Isometric_Types = require("../Graphics.Isometric.Types");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var vector = function (v) {
    return function (v1) {
        return {
            x: v1.x - v.x, 
            y: v1.y - v.y, 
            z: v1.z - v.z
        };
    };
};
var translateZ = function (dz) {
    return function (v) {
        return {
            x: v.x, 
            y: v.y, 
            z: v.z + dz
        };
    };
};
var translateY = function (dy) {
    return function (v) {
        return {
            x: v.x, 
            y: v.y + dy, 
            z: v.z
        };
    };
};
var translateX = function (dx) {
    return function (v) {
        return {
            x: v.x + dx, 
            y: v.y, 
            z: v.z
        };
    };
};
var translate = function (v) {
    return function (v1) {
        return {
            x: v.x + v1.x, 
            y: v.y + v1.y, 
            z: v.z + v1.z
        };
    };
};
var scale = function (f) {
    return function (v) {
        return {
            x: f * v.x, 
            y: f * v.y, 
            z: f * v.z
        };
    };
};
var rotateZ = function (phi) {
    return function (v) {
        return {
            x: $$Math.cos(phi) * v.x - $$Math.sin(phi) * v.y, 
            y: $$Math.sin(phi) * v.x + $$Math.cos(phi) * v.y, 
            z: v.z
        };
    };
};
var rotateY = function (phi) {
    return function (v) {
        return {
            x: $$Math.cos(phi) * v.x + $$Math.sin(phi) * v.z, 
            y: v.y, 
            z: -$$Math.sin(phi) * v.x + $$Math.cos(phi) * v.z
        };
    };
};
var rotateX = function (phi) {
    return function (v) {
        return {
            x: v.x, 
            y: $$Math.cos(phi) * v.y - $$Math.sin(phi) * v.z, 
            z: $$Math.sin(phi) * v.y + $$Math.cos(phi) * v.z
        };
    };
};
var point = function (x) {
    return function (y) {
        return function (z) {
            return {
                x: x, 
                y: y, 
                z: z
            };
        };
    };
};
var origin = {
    x: 0.0, 
    y: 0.0, 
    z: 0.0
};
var from2D = function (v) {
    return {
        x: v.x, 
        y: v.y, 
        z: 0.0
    };
};
var dot = function (v) {
    return function (v1) {
        return v.x * v1.x + v.y * v1.y + v.z * v1.z;
    };
};
var norm = function (p) {
    return $$Math.sqrt(dot(p)(p));
};
var normalize = function (p) {
    var n = norm(p);
    return {
        x: p.x / n, 
        y: p.y / n, 
        z: p.z / n
    };
};
var depth = function (p) {
    return p.x + p.y + p.z;
};
var cross = function (v) {
    return function (v1) {
        return {
            x: v.y * v1.z - v.z * v1.y, 
            y: v.z * v1.x - v.x * v1.z, 
            z: v.x * v1.y - v.y * v1.x
        };
    };
};
module.exports = {
    cross: cross, 
    depth: depth, 
    dot: dot, 
    from2D: from2D, 
    norm: norm, 
    normalize: normalize, 
    origin: origin, 
    point: point, 
    rotateX: rotateX, 
    rotateY: rotateY, 
    rotateZ: rotateZ, 
    scale: scale, 
    translate: translate, 
    translateX: translateX, 
    translateY: translateY, 
    translateZ: translateZ, 
    vector: vector
};

},{"../Data.EuclideanRing":80,"../Data.Ring":126,"../Data.Semiring":130,"../Graphics.Drawing":159,"../Graphics.Isometric.Types":162,"../Math":166,"../Prelude":171}],162:[function(require,module,exports){
arguments[4][54][0].apply(exports,arguments)
},{"dup":54}],163:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Color = require("../Color");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_List = require("../Data.List");
var Data_List_ThreeOrMore = require("../Data.List.ThreeOrMore");
var Data_List_Types = require("../Data.List.Types");
var Data_Monoid = require("../Data.Monoid");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Graphics_Drawing = require("../Graphics.Drawing");
var Graphics_Isometric_Point = require("../Graphics.Isometric.Point");
var Graphics_Isometric_Types = require("../Graphics.Isometric.Types");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Fill = (function () {
    function Fill(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Fill.create = function (value0) {
        return function (value1) {
            return new Fill(value0, value1);
        };
    };
    return Fill;
})();
var Many = (function () {
    function Many(value0) {
        this.value0 = value0;
    };
    Many.create = function (value0) {
        return new Many(value0);
    };
    return Many;
})();
var transform = function (t) {
    var go = function (v) {
        if (v instanceof Fill) {
            return new Fill(v.value0, Data_Functor.map(Data_List_Types.functorList)(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(t))(v.value1));
        };
        if (v instanceof Many) {
            return new Many(Data_Functor.map(Data_List_Types.functorList)(transform(t))(v.value0));
        };
        throw new Error("Failed pattern match at Graphics.Isometric line 149, column 5 - line 149, column 57: " + [ v.constructor.name ]);
    };
    return go;
};
var translateX = function (dx) {
    return transform(Graphics_Isometric_Point.translateX(dx));
};
var translateY = function (dy) {
    return transform(Graphics_Isometric_Point.translateY(dy));
};
var translateZ = function (dz) {
    return transform(Graphics_Isometric_Point.translateZ(dz));
};
var semigroupScene = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Many) {
            return new Many(Data_Semigroup.append(Data_List_Types.semigroupList)(v.value0)(Data_List.singleton(v1)));
        };
        if (v1 instanceof Many) {
            return new Many(new Data_List_Types.Cons(v, v1.value0));
        };
        return new Many(new Data_List_Types.Cons(v, new Data_List_Types.Cons(v1, Data_List_Types.Nil.value)));
    };
});
var scale = function (factor) {
    return transform(Graphics_Isometric_Point.scale(factor));
};
var rotateZ = function (angle) {
    return transform(Graphics_Isometric_Point.rotateZ(angle));
};
var rotateY = function (angle) {
    return transform(Graphics_Isometric_Point.rotateY(angle));
};
var rotateX = function (angle) {
    return transform(Graphics_Isometric_Point.rotateX(angle));
};
var polygon = function (num) {
    return function (r) {
        var toPoint = function (j) {
            var phi = ((-2.0 * $$Math.pi) / Data_Int.toNumber(num)) * Data_Int.toNumber(j);
            return {
                x: r * $$Math.cos(phi), 
                y: r * $$Math.sin(phi)
            };
        };
        return Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(toPoint)(new Data_NonEmpty.NonEmpty(0, new Data_NonEmpty.NonEmpty(1, new Data_NonEmpty.NonEmpty(2, Data_List.range(3)(num - 1 | 0)))));
    };
};
var move = function (v) {
    return Data_Functor.map(Data_List_Types.functorList)(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(Graphics_Isometric_Point.translate(v)));
};
var monoidScene = new Data_Monoid.Monoid(function () {
    return semigroupScene;
}, new Many(Data_Monoid.mempty(Data_List_Types.monoidList)));
var isometricAngle = $$Math.pi / 2.0 - $$Math.asin(1.0 / $$Math.sqrt(3.0));
var project = function (p) {
    var beta = $$Math.pi / 4.0;
    var rotated = Graphics_Isometric_Point.rotateX(isometricAngle)(Graphics_Isometric_Point.rotateZ(beta)(p));
    return {
        x: -rotated.x, 
        y: rotated.y
    };
};
var renderFace = function (dir) {
    return function (color) {
        return function (v) {
            var path = Graphics_Drawing.closed(Data_NonEmpty.foldableNonEmpty(Data_NonEmpty.foldableNonEmpty(Data_NonEmpty.foldableNonEmpty(Data_List_Types.foldableList))))(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(project)(v));
            var normal = Graphics_Isometric_Point.normalize(Graphics_Isometric_Point.cross(Graphics_Isometric_Point.vector(v.value0)(v.value1.value0))(Graphics_Isometric_Point.vector(v.value0)(v.value1.value1.value0)));
            var amount = 0.2 * Graphics_Isometric_Point.dot(dir)(normal);
            var col = Color.lighten(amount)(color);
            return Graphics_Drawing.filled(Graphics_Drawing.fillColor(col))(path);
        };
    };
};
var filled = Fill.create;
var fillShape = function (dir) {
    return function (color) {
        return function (faces) {
            var totalDepth = function (face) {
                return Data_Foldable.sum(Data_NonEmpty.foldableNonEmpty(Data_NonEmpty.foldableNonEmpty(Data_NonEmpty.foldableNonEmpty(Data_List_Types.foldableList))))(Data_Semiring.semiringNumber)(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(Graphics_Isometric_Point.depth)(face));
            };
            var sortedFaces = Data_List.sortBy(Data_Ord.comparing(Data_Ord.ordNumber)(totalDepth))(faces);
            return Data_Foldable.foldMap(Data_List_Types.foldableList)(Graphics_Drawing.monoidDrawing)(renderFace(dir)(color))(sortedFaces);
        };
    };
};
var renderScene = function (dir) {
    return function (scene) {
        var dir$prime = Graphics_Isometric_Point.normalize(dir);
        var go = function (v) {
            if (v instanceof Fill) {
                return fillShape(dir$prime)(v.value0)(v.value1);
            };
            if (v instanceof Many) {
                return Data_Foldable.foldMap(Data_List_Types.foldableList)(Graphics_Drawing.monoidDrawing)(go)(v.value0);
            };
            throw new Error("Failed pattern match at Graphics.Isometric line 211, column 25 - line 215, column 26: " + [ v.constructor.name ]);
        };
        return go(scene);
    };
};
var extrudeCone = function (path2D) {
    return function (height) {
        var tip = {
            x: 0.0, 
            y: 0.0, 
            z: height
        };
        var side = function (p1) {
            return function (p2) {
                return new Data_NonEmpty.NonEmpty(p1, new Data_NonEmpty.NonEmpty(p2, new Data_NonEmpty.NonEmpty(tip, Data_List_Types.Nil.value)));
            };
        };
        var path = Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(Graphics_Isometric_Point.from2D)(path2D);
        var sides = Data_List.zipWith(side)(Data_List_ThreeOrMore.toList(path))(new Data_List_Types.Cons(Data_List_ThreeOrMore.last(path), Data_List_ThreeOrMore.toList(path)));
        return new Data_List_Types.Cons(path, sides);
    };
};
var extrude = function (path2D) {
    return function (height) {
        var raise = Graphics_Isometric_Point.translateZ(height);
        var side = function (p1) {
            return function (p2) {
                return new Data_NonEmpty.NonEmpty(p1, new Data_NonEmpty.NonEmpty(p2, new Data_NonEmpty.NonEmpty(raise(p2), new Data_List_Types.Cons(raise(p1), Data_List_Types.Nil.value))));
            };
        };
        var path = Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(Graphics_Isometric_Point.from2D)(path2D);
        var sides = Data_List.zipWith(side)(Data_List_ThreeOrMore.toList(path))(new Data_List_Types.Cons(Data_List_ThreeOrMore.last(path), Data_List_ThreeOrMore.toList(path)));
        var top = Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_NonEmpty.functorNonEmpty(Data_List_Types.functorList))))(raise)(path);
        return new Data_List_Types.Cons(path, new Data_List_Types.Cons(Data_List_ThreeOrMore.reverse(top), sides));
    };
};
var prism = function (p) {
    return function (dx) {
        return function (dy) {
            return function (dz) {
                var rectangle = new Data_NonEmpty.NonEmpty({
                    x: 0.0, 
                    y: 0.0
                }, new Data_NonEmpty.NonEmpty({
                    x: 0.0, 
                    y: dy
                }, new Data_NonEmpty.NonEmpty({
                    x: dx, 
                    y: dy
                }, new Data_List_Types.Cons({
                    x: dx, 
                    y: 0.0
                }, Data_List_Types.Nil.value))));
                return move(p)(extrude(rectangle)(dz));
            };
        };
    };
};
var cylinder = function (p) {
    return function (num) {
        return function (r) {
            return function (height) {
                return move(p)(extrude(polygon(num)(r))(height));
            };
        };
    };
};
var cube = function (p) {
    return function (dl) {
        return prism(p)(dl)(dl)(dl);
    };
};
var cone = function (p) {
    return function (num) {
        return function (r) {
            return function (height) {
                return move(p)(extrudeCone(polygon(num)(r))(height));
            };
        };
    };
};
var pyramid = function (p) {
    return cone(p)(4);
};
var pyramid3 = function (p) {
    return function (len) {
        return cone(p)(3)((len * $$Math.sqrt(3.0)) / 3.0);
    };
};
module.exports = {
    Fill: Fill, 
    Many: Many, 
    cone: cone, 
    cube: cube, 
    cylinder: cylinder, 
    extrude: extrude, 
    extrudeCone: extrudeCone, 
    filled: filled, 
    prism: prism, 
    pyramid: pyramid, 
    pyramid3: pyramid3, 
    renderScene: renderScene, 
    rotateX: rotateX, 
    rotateY: rotateY, 
    rotateZ: rotateZ, 
    scale: scale, 
    translateX: translateX, 
    translateY: translateY, 
    translateZ: translateZ, 
    semigroupScene: semigroupScene, 
    monoidScene: monoidScene
};

},{"../Color":3,"../Data.EuclideanRing":80,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Int":101,"../Data.List":105,"../Data.List.ThreeOrMore":103,"../Data.List.Types":104,"../Data.Monoid":116,"../Data.NonEmpty":119,"../Data.Ord":123,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Graphics.Drawing":159,"../Graphics.Isometric.Point":161,"../Graphics.Isometric.Types":162,"../Math":166,"../Prelude":171}],164:[function(require,module,exports){
"use strict";
var Color = require("../Color");
var Color_Scheme_MaterialDesign = require("../Color.Scheme.MaterialDesign");
var Control_Alternative = require("../Control.Alternative");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Data_Array = require("../Data.Array");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Flare = require("../Flare");
var Flare_Drawing = require("../Flare.Drawing");
var Graphics_Drawing = require("../Graphics.Drawing");
var Graphics_Isometric = require("../Graphics.Isometric");
var Graphics_Isometric_DepthSort = require("../Graphics.Isometric.DepthSort");
var Graphics_Isometric_Point = require("../Graphics.Isometric.Point");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Signal_DOM = require("../Signal.DOM");

/**
 * 
 * for (var z = 0; z < 32; z++) {
 *     for (var y = 0; y < 32; y++) {
 *       for (var x = 32 - 1; x >= 0; x--) {
 *         const color = {
 *           r: z * 8,
 *           g: y * 8,
 *           b: 255 - x * 8,
 *         }
 * 
 *         swatch.push(color)
 *       }
 *     }
 *   }
 */
var size = 6.0;

// Example 3
var p = function (x) {
    return function (y) {
        return function (z) {
            return {
                x: Data_Int.toNumber(x), 
                y: Data_Int.toNumber(y), 
                z: Data_Int.toNumber(z)
            };
        };
    };
};
var intSize = Data_Int.floor(size);
var coloring = function (v) {
    return Color["rgb'"](v.z / size)(v.y / size)(v.x / size);
};
var colorcube = function (angle) {
    return function (sliceX) {
        return function (sliceY) {
            return function (sliceZ) {
                return Graphics_Drawing.translate(300.0)(300.0)(Graphics_Isometric.renderScene({
                    x: -4.0, 
                    y: -1.0, 
                    z: 3.0
                })(Graphics_Isometric.scale(45.0)(Graphics_Isometric.rotateZ(angle)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Graphics_Isometric.monoidScene)(function (pos) {
                    return Graphics_Isometric.filled(coloring(pos))(Graphics_Isometric.cube(pos)(1.0));
                })(Control_Bind.bind(Control_Bind.bindArray)(Data_Array.range(1)(sliceX))(function (v) {
                    return Control_Bind.bind(Control_Bind.bindArray)(Data_Array.range(1)(sliceY))(function (v1) {
                        return Control_Bind.bind(Control_Bind.bindArray)(Data_Array.range(1)(sliceZ))(function (v2) {
                            return Control_Applicative.pure(Control_Applicative.applicativeArray)(p(v)(v1)(v2));
                        });
                    });
                }))))));
            };
        };
    };
};

// [ p 1 1 0 , p 1 2 0 , p 1 3 0 , p 2 3 0

// , p 3 3 0 , p 0 (-2) 0 , p 1 (-2) 0 , p 2 (-2) 0

// , p 3 (-2) 0 , p 3 (-1) 0 , p 4 (-1) 0 , p 5 (-1) 0

// , p 5 (-1) 0 , p 5 0 0 , p 4 2 0 , p 5 1 0

// , p 5 2 0 , p 4 3 0 , p 1 0 1 , p 1 1 1

// ]
var main = Flare_Drawing.runFlareDrawing("controls3")("canvas3")(Control_Apply.apply(Flare.applyUI)(Control_Apply.apply(Flare.applyUI)(Control_Apply.apply(Flare.applyUI)(Data_Functor.map(Flare.functorUI)(colorcube)(Flare.numberSlider("Rotation")(-0.25 * $$Math.pi)(0.25 * $$Math.pi)(1.0e-2)(0.0)))(Flare.intSlider("Slice X")(1)(intSize)(intSize)))(Flare.intSlider("Slice Y")(1)(intSize)(intSize)))(Flare.intSlider("Slice Z")(1)(intSize)(intSize)));
module.exports = {
    colorcube: colorcube, 
    coloring: coloring, 
    intSize: intSize, 
    main: main, 
    p: p, 
    size: size
};

},{"../Color":3,"../Color.Scheme.MaterialDesign":2,"../Control.Alternative":5,"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Data.Array":53,"../Data.EuclideanRing":80,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Int":101,"../Data.Ring":126,"../Data.Semiring":130,"../Flare":153,"../Flare.Drawing":151,"../Graphics.Drawing":159,"../Graphics.Isometric":163,"../Graphics.Isometric.DepthSort":160,"../Graphics.Isometric.Point":161,"../Math":166,"../Prelude":171,"../Signal.DOM":175}],165:[function(require,module,exports){
"use strict";

// module Math

exports.abs = Math.abs;

exports.acos = Math.acos;

exports.asin = Math.asin;

exports.atan = Math.atan;

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

exports.ceil = Math.ceil;

exports.cos = Math.cos;

exports.exp = Math.exp;

exports.floor = Math.floor;

exports.trunc = Math.trunc || function (n) {
  return n < 0 ? Math.ceil(n) : Math.floor(n);
};

exports.log = Math.log;

exports.max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

exports.min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports.remainder = function (n) {
  return function (m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;

exports.tan = Math.tan;

exports.e = Math.E;

exports.ln2 = Math.LN2;

exports.ln10 = Math.LN10;

exports.log2e = Math.LOG2E;

exports.log10e = Math.LOG10E;

exports.pi = Math.PI;

exports.sqrt1_2 = Math.SQRT1_2;

exports.sqrt2 = Math.SQRT2;

},{}],166:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    abs: $foreign.abs, 
    acos: $foreign.acos, 
    asin: $foreign.asin, 
    atan: $foreign.atan, 
    atan2: $foreign.atan2, 
    ceil: $foreign.ceil, 
    cos: $foreign.cos, 
    e: $foreign.e, 
    exp: $foreign.exp, 
    floor: $foreign.floor, 
    ln10: $foreign.ln10, 
    ln2: $foreign.ln2, 
    log: $foreign.log, 
    log10e: $foreign.log10e, 
    log2e: $foreign.log2e, 
    max: $foreign.max, 
    min: $foreign.min, 
    pi: $foreign.pi, 
    pow: $foreign.pow, 
    remainder: $foreign.remainder, 
    round: $foreign.round, 
    sin: $foreign.sin, 
    sqrt: $foreign.sqrt, 
    sqrt1_2: $foreign.sqrt1_2, 
    sqrt2: $foreign.sqrt2, 
    tan: $foreign.tan, 
    trunc: $foreign.trunc
};

},{"./foreign":165}],167:[function(require,module,exports){
"use strict";

// module Partial.Unsafe

exports.unsafePartial = function (f) {
  return f();
};

exports.unsafePartialBecause = function (reason) {
  return function (f) {
    try {
      return exports.unsafePartial(f);
    } catch (err) {
      throw new Error("unsafePartial failed. The following " +
                      "assumption was incorrect: '" + reason + "'.");
    }
  };
};

},{}],168:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Partial = require("../Partial");
var unsafeCrashWith = function (msg) {
    return $foreign.unsafePartial(function (dictPartial) {
        return Partial.crashWith(dictPartial)(msg);
    });
};
module.exports = {
    unsafeCrashWith: unsafeCrashWith, 
    unsafePartial: $foreign.unsafePartial, 
    unsafePartialBecause: $foreign.unsafePartialBecause
};

},{"../Partial":170,"./foreign":167}],169:[function(require,module,exports){
"use strict";

// module Partial

exports.crashWith = function () {
  return function (msg) {
    throw new Error(msg);
  };
};

},{}],170:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var crash = function (dictPartial) {
    return $foreign.crashWith(dictPartial)("Partial.crash: partial function");
};
module.exports = {
    crash: crash, 
    crashWith: $foreign.crashWith
};

},{"./foreign":169}],171:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra");
var Data_Bounded = require("../Data.Bounded");
var Data_CommutativeRing = require("../Data.CommutativeRing");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Field = require("../Data.Field");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_NaturalTransformation = require("../Data.NaturalTransformation");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Data_Void = require("../Data.Void");
module.exports = {};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Bind":12,"../Control.Category":13,"../Control.Monad":38,"../Control.Semigroupoid":42,"../Data.Boolean":63,"../Data.BooleanAlgebra":64,"../Data.Bounded":66,"../Data.CommutativeRing":71,"../Data.Eq":78,"../Data.EuclideanRing":80,"../Data.Field":81,"../Data.Function":88,"../Data.Functor":91,"../Data.HeytingAlgebra":96,"../Data.NaturalTransformation":117,"../Data.Ord":123,"../Data.Ordering":124,"../Data.Ring":126,"../Data.Semigroup":128,"../Data.Semiring":130,"../Data.Show":132,"../Data.Unit":149,"../Data.Void":150}],172:[function(require,module,exports){
// module Signal.Channel

exports.channelP =
  function channelP(constant) {
    return function(v) {
      return function() {
        return constant(v);
      };
    };
  };

exports.sendP =
  function sendP(chan) {
    return function(v) {
      return function() {
        chan.set(v);
      };
    };
  };

exports.subscribe =
  function subscribe(chan) {
    return chan;
  };

},{}],173:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var send = $foreign.sendP;
var channel = $foreign.channelP(Signal.constant);
module.exports = {
    channel: channel, 
    send: send, 
    subscribe: $foreign.subscribe
};

},{"../Control.Monad.Eff":27,"../Prelude":171,"../Signal":179,"./foreign":172}],174:[function(require,module,exports){
// module Signal.DOM

exports.keyPressedP =
  function keyPressedP(constant) {
    return function(keyCode) {
      return function() {
        var out = constant(false);
        window.addEventListener("keydown", function(e) {
          if (e.keyCode === keyCode) out.set(true);
        });
        window.addEventListener("keyup", function(e) {
          if (e.keyCode === keyCode) out.set(false);
        });
        return out;
      };
    };
  };

exports.mouseButtonP =
  function mouseButtonP(constant) {
    return function(button) {
      return function() {
        var out = constant(false);
        window.addEventListener("mousedown", function(e) {
          if (e.button === button) out.set(true);
        });
        window.addEventListener("mouseup", function(e) {
          if (e.button === button) out.set(false);
        });
        return out;
      };
    };
  };

exports.touchP =
  function touchP(constant) {
    var out = constant([]);
    function report(e) {
      var touches = [], i, l = e.touches.length;
      for (i = 0; i < l; i++) touches.push(e.touches.item(i));
      out.set(touches);
    }
    window.addEventListener("touchstart", report);
    window.addEventListener("touchend", report);
    window.addEventListener("touchmove", report);
    window.addEventListener("touchcancel", report);
    return function() {
      return out;
    };
  };

exports.mousePosP =
  function mousePosP(constant) {
    var out = constant({x:0,y:0});
    window.addEventListener('mousemove', function(e) {
      if (e.pageX !== undefined && e.pageY !== undefined) {
        out.set({x: e.pageX, y: e.pageY});
      } else if (e.clientX !== undefined && e.clientY !== undefined) {
        out.set({
          x: e.clientX + document.body.scrollLeft +
             document.documentElement.scrollLeft,
          y: e.clientY + document.body.scrollTop +
             document.documentElement.scrollTop
        });
      } else {
        throw new Error('Mouse event has no coordinates I recognise!');
      }
    });
    return function() {
      return out;
    };
  };

exports.animationFrameP =
  function animationFrameP(constant) {
    return function(now) {
      return function() {
        var requestAnimFrame, cancelAnimFrame;
        if (window.requestAnimationFrame) {
          requestAnimFrame = window.requestAnimationFrame;
          cancelAnimFrame = window.cancelAnimationFrame;
        } else if (window.mozRequestAnimationFrame) {
          requestAnimFrame = window.mozRequestAnimationFrame;
          cancelAnimFrame = window.mozCancelAnimationFrame;
        } else if (window.webkitRequestAnimationFrame) {
          requestAnimFrame = window.webkitRequestAnimationFrame;
          cancelAnimFrame = window.webkitCancelAnimationFrame;
        } else if (window.msRequestAnimationFrame) {
          requestAnimFrame = window.msRequestAnimationFrame;
          cancelAnimFrame = window.msCancelAnimationFrame;
        } else if (window.oRequestAnimationFrame) {
          requestAnimFrame = window.oRequestAnimationFrame;
          cancelAnimFrame = window.oCancelAnimationFrame;
        } else {
          requestAnimFrame = function(cb) {setTimeout(function() {cb(now())}, 1000/60)};
          cancelAnimFrame = window.clearTimeout;
        }
        var out = constant(now());
        requestAnimFrame(function tick(t) {
          out.set(t); requestAnimFrame(tick);
        });
        return out;
      };
    };
  };

exports.windowDimensionsP = function windowDimensionsP(constant) {
  var out = constant({ w: window.innerWidth, h: window.innerHeight });
  window.addEventListener("resize", function() {
    out.set({ w: window.innerWidth, h: window.innerHeight });
  });
  return function() {
    return out;
  }
}

},{}],175:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var DOM = require("../DOM");
var Data_Function = require("../Data.Function");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_Time = require("../Signal.Time");
var windowDimensions = $foreign.windowDimensionsP(Signal.constant);
var touch = $foreign.touchP(Signal.constant);
var tap = function __do() {
    var v = touch();
    return Signal.flippedMap(Signal.functorSignal)(v)(function (t) {
        if (t.length === 0) {
            return false;
        };
        return true;
    });
};
var mousePos = $foreign.mousePosP(Signal.constant);
var mouseButton = $foreign.mouseButtonP(Signal.constant);
var keyPressed = $foreign.keyPressedP(Signal.constant);
var animationFrame = $foreign.animationFrameP(Signal.constant)(Signal_Time.now);
module.exports = {
    animationFrame: animationFrame, 
    keyPressed: keyPressed, 
    mouseButton: mouseButton, 
    mousePos: mousePos, 
    tap: tap, 
    touch: touch, 
    windowDimensions: windowDimensions
};

},{"../Control.Applicative":6,"../Control.Bind":12,"../Control.Monad.Eff":27,"../Control.Monad.Eff.Timer":23,"../DOM":48,"../Data.Function":88,"../Prelude":171,"../Signal":179,"../Signal.Time":177,"./foreign":174}],176:[function(require,module,exports){
(function (process){
// module Signal.Time

function now() {
  var perf = typeof performance !== 'undefined' ? performance : null,
      proc = typeof process !== 'undefined' ? process : null;
  return (
    perf && (perf.now || perf.webkitNow || perf.msNow || perf.oNow || perf.mozNow) ||
    (proc && proc.hrtime && function() {
      var t = proc.hrtime();
      return (t[0] * 1e9 + t[1]) / 1e6;
    }) ||
    Date.now
  ).call(perf);
};

exports.now = now;

exports.everyP = function everyP(constant) {
  return function(t) {
    var out = constant(now());
    setInterval(function() {
      out.set(now());
    }, t);
    return out;
  };
};

exports.delayP = function delayP(constant) {
  return function(t) {
    return function(sig) {
      var out = constant(sig.get());
      var first = true;
      sig.subscribe(function(val) {
        if (first) {
          first = false;
        } else {
          setTimeout(function() {
            out.set(val);
          }, t);
        }
      });
      return out;
    }
  };
};

exports.sinceP = function sinceP(constant) {
  return function(t) {
    return function(sig) {
      var out = constant(false);
      var first = true;
      var timer = undefined;
      var tick = function() {
        out.set(false);
        timer = undefined;
      };
      sig.subscribe(function() {
        if (first) {
          first = false;
          return;
        }
        if (timer === undefined) {
          out.set(true);
          timer = setTimeout(tick, t);
        } else {
          clearTimeout(timer);
          timer = setTimeout(tick, t);
        }
      });
      return out;
    }
  };
};

}).call(this,require('_process'))
},{"_process":1}],177:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var since = $foreign.sinceP(Signal.constant);
var second = 1000.0;
var millisecond = 1.0;
var every = $foreign.everyP(Signal.constant);
var delay = $foreign.delayP(Signal.constant);
var debounce = function (t) {
    return function (s) {
        var whenEqual = function (value) {
            return function (input) {
                return Signal.filter(Data_Eq.eq(Data_Eq.eqBoolean)(value))(value)(input);
            };
        };
        var whenChangeTo = function (value) {
            return function (input) {
                return whenEqual(value)(Signal.dropRepeats(Data_Eq.eqBoolean)(input));
            };
        };
        var leading = whenChangeTo(false)(since(t)(s));
        return Signal.sampleOn(leading)(s);
    };
};
module.exports = {
    debounce: debounce, 
    delay: delay, 
    every: every, 
    millisecond: millisecond, 
    second: second, 
    since: since, 
    now: $foreign.now
};

},{"../Control.Monad.Eff":27,"../Control.Monad.Eff.Timer":23,"../Data.Eq":78,"../Data.Function":88,"../Prelude":171,"../Signal":179,"./foreign":176}],178:[function(require,module,exports){
// module Signal

function make(initial) {
  var subs = [];
  var val = initial;
  var sig = {
    subscribe: function(sub) {
      subs.push(sub);
      sub(val);
    },
    get: function() { return val; },
    set: function(newval) {
      val = newval;
      subs.forEach(function(sub) { sub(newval); });
    }
  };
  return sig;
};

exports.constant = make;

exports.mapSig = function(fun) {
  return function(sig) {
    var out = make(fun(sig.get()));
    sig.subscribe(function(val) { out.set(fun(val)); });
    return out;
  };
};


exports.applySig = function(fun) {
  return function(sig) {
    var out = make(fun.get()(sig.get()));
    var produce = function() { out.set(fun.get()(sig.get())); };
    fun.subscribe(produce);
    sig.subscribe(produce);
    return out;
  };
};

exports.merge = function(sig1) {
  return function(sig2) {
    var out = make(sig1.get());
    sig2.subscribe(out.set);
    sig1.subscribe(out.set);
    return out;
  };
};

exports.foldp = function(fun) {
  return function(seed) {
    return function(sig) {
      var acc = seed;
      var out = make(acc);
      sig.subscribe(function(val) {
        acc = fun(val)(acc);
        out.set(acc);
      });
      return out;
    };
  };
};

exports.sampleOn = function(sig1) {
  return function(sig2) {
    var out = make(sig2.get());
    sig1.subscribe(function() {
      out.set(sig2.get());
    });
    return out;
  };
};

exports.dropRepeats = function(eq) {
  return function(sig) {
    var val = sig.get();
    var out = make(val);
    sig.subscribe(function(newval) {
      if (!eq["eq"](val)(newval)) {
        val = newval;
        out.set(val);
      }
    });
    return out;
  };
};

exports["dropRepeats'"] = function(sig) {
  var val = sig.get();
  var out = make(val);
  sig.subscribe(function(newval) {
    if (val !== newval) {
      val = newval;
      out.set(val);
    }
  });
  return out;
};

exports.runSignal =
  function runSignal(sig) {
    return function() {
      sig.subscribe(function(val) {
        val();
      });
      return {};
    };
  };

exports.unwrap = function(sig) {
  return function() {
    var out = make(sig.get()());
    sig.subscribe(function(val) { out.set(val()); });
    return out;
  };
};

exports.filter = function(fn) {
  return function(seed) {
    return function(sig) {
      var out = make(fn(sig.get()) ? sig.get() : seed);
      sig.subscribe(function(val) { if (fn(val)) out.set(val); });
      return out;
    };
  };
};

exports.flattenArray = function(sig) {
  return function(seed) {
    var first = sig.get().slice();
    if (first.length > 0) {
      seed = first[0];
    } else {
      first = null;
    }
    var out = make(seed);
    var feed = function(items) { items.forEach(out.set); };
    setTimeout(function() { sig.subscribe(function(val) {
      if (first === null) {
        feed(val);
      } else {
        feed(first.slice(1));
        first = null;
      }
    }); }, 0);
    return out;
  };
};

},{}],179:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Prelude = require("../Prelude");
var squigglyMap = function (dictFunctor) {
    return Data_Functor.map(dictFunctor);
};
var squigglyApply = function (dictApply) {
    return Control_Apply.apply(dictApply);
};
var semigroupSignal = new Data_Semigroup.Semigroup($foreign.merge);
var monoidSignal = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupSignal;
    }, $foreign.constant(Data_Monoid.mempty(dictMonoid)));
};
var mergeMany = function (dictFunctor) {
    return function (dictFoldable) {
        return function (sigs) {
            var mergeMaybe = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return v;
                    };
                    if (v instanceof Data_Maybe.Nothing) {
                        return v1;
                    };
                    if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                        return new Data_Maybe.Just($foreign.merge(v.value0)(v1.value0));
                    };
                    throw new Error("Failed pattern match at Signal line 53, column 9 - line 53, column 33: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Foldable.foldl(dictFoldable)(mergeMaybe)(Data_Maybe.Nothing.value)(Data_Functor.map(dictFunctor)(Data_Maybe.Just.create)(sigs));
        };
    };
};
var functorSignal = new Data_Functor.Functor($foreign.mapSig);
var flippedMap = function (dictFunctor) {
    return Data_Function.flip(Data_Functor.map(dictFunctor));
};
var flatten = function (dictFunctor) {
    return function (dictFoldable) {
        return function (sig) {
            return $foreign.flattenArray(flippedMap(functorSignal)(sig)(function ($14) {
                return Data_Foldable.fold(dictFoldable)(Data_Monoid.monoidArray)(Data_Functor.map(dictFunctor)(function (i) {
                    return [ i ];
                })($14));
            }));
        };
    };
};
var filterMap = function (f) {
    return function (def) {
        return function (sig) {
            return Data_Functor.map(functorSignal)(Data_Maybe.fromMaybe(def))($foreign.filter(Data_Maybe.isJust)(new Data_Maybe.Just(def))(Data_Functor.map(functorSignal)(f)(sig)));
        };
    };
};
var applySignal = new Control_Apply.Apply(function () {
    return functorSignal;
}, $foreign.applySig);
var map2 = function (f) {
    return function (a) {
        return function (b) {
            return squigglyApply(applySignal)(squigglyMap(functorSignal)(f)(a))(b);
        };
    };
};
var map3 = function (f) {
    return function (a) {
        return function (b) {
            return function (c) {
                return squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyMap(functorSignal)(f)(a))(b))(c);
            };
        };
    };
};
var map4 = function (f) {
    return function (a) {
        return function (b) {
            return function (c) {
                return function (d) {
                    return squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyMap(functorSignal)(f)(a))(b))(c))(d);
                };
            };
        };
    };
};
var map5 = function (f) {
    return function (a) {
        return function (b) {
            return function (c) {
                return function (d) {
                    return function (e) {
                        return squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyApply(applySignal)(squigglyMap(functorSignal)(f)(a))(b))(c))(d))(e);
                    };
                };
            };
        };
    };
};
var applicativeSignal = new Control_Applicative.Applicative(function () {
    return applySignal;
}, $foreign.constant);
module.exports = {
    filterMap: filterMap, 
    flatten: flatten, 
    flippedMap: flippedMap, 
    map2: map2, 
    map3: map3, 
    map4: map4, 
    map5: map5, 
    mergeMany: mergeMany, 
    squigglyApply: squigglyApply, 
    squigglyMap: squigglyMap, 
    functorSignal: functorSignal, 
    applySignal: applySignal, 
    applicativeSignal: applicativeSignal, 
    semigroupSignal: semigroupSignal, 
    monoidSignal: monoidSignal, 
    constant: $foreign.constant, 
    dropRepeats: $foreign.dropRepeats, 
    "dropRepeats'": $foreign["dropRepeats'"], 
    filter: $foreign.filter, 
    flattenArray: $foreign.flattenArray, 
    foldp: $foreign.foldp, 
    merge: $foreign.merge, 
    runSignal: $foreign.runSignal, 
    sampleOn: $foreign.sampleOn, 
    unwrap: $foreign.unwrap
};

},{"../Control.Applicative":6,"../Control.Apply":8,"../Control.Monad.Eff":27,"../Control.Semigroupoid":42,"../Data.Foldable":83,"../Data.Function":88,"../Data.Functor":91,"../Data.Maybe":109,"../Data.Monoid":116,"../Data.Semigroup":128,"../Prelude":171,"./foreign":178}],180:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var Proxy3 = (function () {
    function Proxy3() {

    };
    Proxy3.value = new Proxy3();
    return Proxy3;
})();
var Proxy2 = (function () {
    function Proxy2() {

    };
    Proxy2.value = new Proxy2();
    return Proxy2;
})();
var $$Proxy = (function () {
    function $$Proxy() {

    };
    $$Proxy.value = new $$Proxy();
    return $$Proxy;
})();
module.exports = {
    "Proxy": $$Proxy, 
    Proxy2: Proxy2, 
    Proxy3: Proxy3
};

},{}],181:[function(require,module,exports){
"use strict";

// module Unsafe.Coerce

exports.unsafeCoerce = function (x) {
  return x;
};

},{}],182:[function(require,module,exports){
// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
module.exports = {
    unsafeCoerce: $foreign.unsafeCoerce
};

},{"./foreign":181}],183:[function(require,module,exports){
require('Main').main();

},{"Main":164}]},{},[183]);
