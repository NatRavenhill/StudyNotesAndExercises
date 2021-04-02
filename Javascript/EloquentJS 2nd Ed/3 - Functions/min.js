//Reimplementing Math.min
var min = function(n1, n2){
    if (n2 > n1){
	return n2;
    }
    else {
	return n1;
    }
}

console.log(min(0,10));
console.log(min(0,-10));
console.log(min(0,0));
console.log(min(23));
