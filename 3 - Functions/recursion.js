/* Defines a recursive function isEven corresponding to this description. The function should accept a number parameter and return a Boolean*/

var isEven = function(n){
    if(n === 0){
	return true;
    }
    else if (n === 1){
	return false;
    }
    //fix for negative numbers
    else if(n < 0){
	return(isEven (-n));
    }
    else{
	return isEven(n-2);
    }
}

console.log(isEven(50));
console.log(isEven(75));
console.log(isEven(-1));
