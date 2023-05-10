void setup(){
  size(800, 800);
  frameRate(30);
}

void draw(){
  background(100);
  if(mousePressed == true){
    fill(255);
  } else {
    fill(0);
  }
  
  rect(25, 25, 50, 50);
}
