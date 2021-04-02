function MultiplicatorUnitFailure() {}

function primitiveMultiply(a, b) {
  if (Math.random() < 0.5)
    return a * b;
  else
    throw new MultiplicatorUnitFailure();
}

//recursion version
function reliableMultiply(a, b) {
  try {
     return primitiveMultiply(a,b);
  }
  catch(e){
    if(e instanceof MultiplicatorUnitFailure){
       return reliableMultiply(a,b);
    }
    else 
        throw e;
  }
}

console.log(reliableMultiply(8, 8));
// → 64

//for loop version
function reliableMultiplyLoop(a,b){
   for(;;){
    try {
        return primitiveMultiply(a,b);
    }
    catch(e){
      if(!(e instanceof MultiplicatorUnitFailure))
      throw e;  
    }
   }
};

console.log(reliableMultiplyLoop(8, 8));