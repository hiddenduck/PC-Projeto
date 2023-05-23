import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class Triple{
    public float[] floats;
    public char[] chars;

    public Triple(float x, float y, float z){
        this.floats = new float[3];
        this.floats[0] = x;
        this.floats[1] = y;
        this.floats[2] = z;
    }

    public Triple(float x, float y, char z){
        this.floats = new float[2];
        this.chars = new char[1];
        this.floats[0] = x;
        this.floats[1] = y;
        this.chars[0] = z;
    }
}

public class GameState {

    public ReadWriteLock lrw = new ReentrantReadWriteLock();
    public float posX, posY, enemyPosX, enemyPosY, alfa, enemyAlfa;

    public String point, enemyPoint;

    public String gameStatus;

    public Set<Triple> boxes;

    public GameState(){
        this.lrw = new ReentrantReadWriteLock();
        this.point = "0";
        this.enemyPoint = "0";
    }

    public GameState(float posX, float posY, float enemyPosX, float enemyPosY, float alfa, float enemyAlfa,
                     String point, String enemyPoint, String gameStatus){
        this.posX = posX;
        this.posY = posY;
        this.enemyPosX = enemyPosX;
        this.enemyPosY = enemyPosY;
        this.alfa = alfa;
        this.enemyAlfa = enemyAlfa;
        this.point = point;
        this.enemyPoint = enemyPoint;
        this.gameStatus = gameStatus;
        this.boxes = new HashSet<>();
    }

    public void putPos(float x, float y, float alfa, boolean enemy){
        if(!enemy){
            this.posX = x;
            this.posY = y;
            this.alfa = alfa;
        } else{
            this.enemyPosX = x;
            this.enemyPosY = y;
            this.enemyAlfa = alfa;
        }
    }

    public void putPoint(String point, String enemyPoint){
        this.point = point;
        this.enemyPoint = enemyPoint;
    }

    public void putBox(Triple newBox){
        this.boxes.add(newBox);
    }

    public void removeBox(Triple oldBox){
        this.boxes.remove(oldBox);
    }

    public void setGameStatus(String s){
        this.gameStatus = s;
    }

    public GameState copy(){
        return new GameState(this.posX, this.posY, this.enemyPosX, this.enemyPosY,
                this.alfa, this.enemyAlfa, this.point, this.enemyPoint, this.gameStatus);
    }

}