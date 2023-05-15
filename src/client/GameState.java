import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class GameState {

    public ReadWriteLock lrw = new ReentrantReadWriteLock();
    private float posX, posY, enemyPosX, enemyPosY, alfa, enemyAlfa;

    private int point, enemyPoint;

    private char gameStatus;

    public GameState(){
        this.lrw = new ReentrantReadWriteLock();
    }

    public GameState(float posX, float posY, float enemyPosX, float enemyPosY, float alfa, float enemyAlfa,
                     int point, int enemyPoint, char gameStatus){
        this.posX = posX;
        this.posY = posY;
        this.enemyPosX = enemyPosX;
        this.enemyPosY = enemyPosY;
        this.alfa = alfa;
        this.enemyAlfa = enemyAlfa;
        this.point = point;
        this.enemyPoint = enemyPoint;
        this.gameStatus = gameStatus;
    }

    public void putPos(float x, float y, float alfa, boolean enemy){
        if(enemy){
            this.posX = x;
            this.posY = y;
            this.alfa = alfa;
        } else{
            this.enemyPosX = x;
            this.enemyPosY = y;
            this.enemyAlfa = alfa;
        }
    }

    public void putPoint(int point, int enemyPoint){
        this.point = point;
        this.enemyPoint = enemyPoint;
    }

    public void setGameStatus(char c){
        this.gameStatus = c;
    }

    public GameState copy(){
        return new GameState(this.posX, this.posY, this.enemyPosX, this.enemyPosY,
                this.alfa, this.enemyAlfa, this.point, this.enemyPoint, this.gameStatus);
    }

}