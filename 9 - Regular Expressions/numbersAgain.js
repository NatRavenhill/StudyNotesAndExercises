// Fill in this regular expression.
//It must support an optional minus or plus sign in front of the number, the decimal dot, and exponent notation—5e-3 or 1E10— again with an optional sign in front of the exponent. 
//.5 and 5. are valid JavaScript numbers, but a lone dot isn’t.
var number = /^[-+]?(\.\d+|\d+(\.\d*)?)(e[-+]?\d+)?$/i;

// Tests:
["1", "-1", "+15", "1.55", ".5", "5.", "1.3e2", "1E-4",
 "1e+12"].forEach(function(s) {
  if (!number.test(s))
    console.log("Failed to match '" + s + "'");
});
["1a", "+-1", "1.2.3", "1+1", "1e4.5", ".5.", "1f5",
 "."].forEach(function(s) {
  if (number.test(s))
    console.log("Incorrectly accepted '" + s + "'");
});