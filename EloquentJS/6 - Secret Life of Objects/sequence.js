// Your code here.
logFive = function(sequence){
    for(var i = 0; i < 5; i++){
        if(sequence[i] === undefined)
            continue;
        console.log(sequence[i]);
    }
};

//Array sequence
function ArraySeq(array){
 for(var i = 0; i < array.length; i++){
     this[i] = array[i];
 }
 this.pos = i + 1;
 this. array = array;
};


// is there a next element?
ArraySeq.prototype.next = function (){
    if(this.pos < this.array.length - 1){
        this.pos++;
        return true;
    }
    return false;
};

//current element
ArraySeq.prototype.current = function (){
    return array[pos];
}

//Rangeseq
function RangeSeq(start, end){
    var current = start;
    for(var i = 0; i < end - start; i++){
        this[i] = current;
        current++;
    }
    this.start = start;
    this.end = end;
    this.pos = -1;
};

//is there a next element?
RangeSeq.prototype.next = function (){
    if(this,pos < this.to){
        pos++;
        return true;
    }
    return false;
}

// current element
RangeSeq.prototype.current = function (){
    return pos;
}

logFive(new ArraySeq([1, 2]));
// → 1
// → 2
logFive(new RangeSeq(100, 1000));
// → 100
// → 101
// → 102
// → 103
// → 104

//answer from book
function logFiveAnswer(sequence) {
    for (var i = 0; i < 5; i++) {
      if (!sequence.next())
        break;
      console.log(sequence.current());
    }
  };

  logFive(new ArraySeq([1,2]));
  logFive(new RangeSeq(100, 1000));