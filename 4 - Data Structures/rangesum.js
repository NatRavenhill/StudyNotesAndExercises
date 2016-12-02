//returns an array of numbers from start to end
function range(start, end, step){
    var nums = [];
    if (step == null){
	step = 1;
    }
    if(step < 0){
	for(var i = start; i >= end;i+=step){
	    nums.push(i);
	}
    }
    else{
	for(var i = start; i <= end;i+=step){
	    nums.push(i);
	}
    }
    return nums;
}

//sum an array of numbers
function sum(nums){
    var sum = 0;
    for(var i = 0; i <= nums.length; i++){
	sum += i;
    }
    return sum;
}
