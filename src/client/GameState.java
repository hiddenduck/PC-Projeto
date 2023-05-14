import java.lang.reflect.Array;
import java.util.List;

public class GameState {
    private float posX, posY, enemyPosX, enemyPosY, alfa, enemyAlfa;

    private int point, enemyPoint;

    private boolean goldenPoint;

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

    public void putGoldenPoint(boolean goldenPoint){
        this.goldenPoint = goldenPoint;
    }

}