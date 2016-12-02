//takes two values and returns true only if they are the same value or are objects with the same properties whose values are also equal

function deepEqual(obj1, obj2){
     //are they not null
    if(obj1 === null || obj2 === null)
	return false;
    //are they the same?
    if (obj1  === obj2)
	return true;
    //are they both objects?
    if(!(typeof obj1 == "object" && typeof obj2 == "object"))
	return false;

    //check properties are the same
    for(var prop in obj1){
	if(!(prop in obj2) || !deepEqual(obj1[prop],obj2[prop]))
	    return false;
    }

    return true;
}
