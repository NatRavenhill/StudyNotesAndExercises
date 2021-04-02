//load ancestry.js first!

var ancestry = JSON.parse(ANCESTRY_FILE);

//calculates average of an array
function average(array) {
    //rewrite function as + is an operator, so cannot be passed
    function plus(a,b) { return a + b; }
    return array.reduce(plus) / array.length;
}

//byName contains an array of all people, indexed by name
var byName = {};
ancestry.forEach(function(person) {
    byName[person.name] = person;
});

//check someone has a mum
function gotMum(person){
    return byName[person.mother] != null;
}

//get age difference between someone and their mum
function getMumAgeDiff(person){
    return person.born - byName[person.mother].born;
}


 //get average age gap of people and their mothers
average(ancestry.filter(gotMum).map(getMumAgeDiff));
