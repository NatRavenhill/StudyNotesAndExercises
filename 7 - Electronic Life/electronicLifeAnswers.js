  function SmartPlantEater() {
    this.energy = 20;
}
SmartPlantEater.prototype.act = function(view){
  var space = view.find(" ");
  //increase energy at which reproduction takes place
  if(this.energy > 50 && space)
      return {type: "reproduce", direction: space};
  var plant = view.find("*");
  var otherPlants = view.findAll("*").length > 0;
  
  //refuse to eat a plant unless 
  //they see at least one other plant nearby
  if(plant && otherPlants && this.energy < 50)
      return {type: "eat", direction: plant};
  if(space)
      return {type: "move", direction: space};
};

var world2 = new LifelikeWorld(
  ["############################",
   "#####                 ######",
   "##   ***                **##",
   "#   *##**         **  O  *##",
   "#    ***     O    ##**    *#",
   "#       O         ##***    #",
   "#                 ##**     #",
   "#   O       #*             #",
   "#*          #**       O    #",
   "#***        ##**    O    **#",
   "##****     ###***       *###",
   "############################"],
  {"#": Wall,
   "O": SmartPlantEater,
   "*": Plant}
);

for (var i = 0; i < 5; i++) {
  world2.turn();
  console.log("turn " + i +":\n");
  console.log(world2.toString());
}

function Tiger(){
  this.energy = 30;
}

Tiger.prototype.act = function(view) {
  var space = view.find(" ");
  if (this.energy > 150 && space)
    return {type: "reproduce", direction: space};
  var herbivore = view.find("O");
  var otherHerbivores = view.findAll("O");
 var anyOtherHerbs = otherHerbivores !== undefined
 && otherHerbivores.length < 1;
  if (herbivore && anyOtherHerbs)
    return {type: "eat", direction: herbivore};
  if (space)
    return {type: "move", direction: space};
};