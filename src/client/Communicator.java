import java.util.Objects;

public class Communicator extends Thread{
    public ConnectionManager connectionManager;
    public GameState gameState;

    public Communicator(ConnectionManager connectionManager, GameState gameState){
        this.connectionManager = connectionManager;
        this.gameState = gameState;
    }
}

class CommunicatorPos extends Communicator{
    private boolean enemy;

    public CommunicatorPos(ConnectionManager connectionManager, GameState gameState, boolean enemy){
        super(connectionManager, gameState);
        this.enemy = enemy;
    }

    public void run(){
        try {
            String pos = this.connectionManager.receive("pos");
            String[] posArgs = pos.split(":", 3);
            this.gameState.putPos(Float.parseFloat(posArgs[0]), Float.parseFloat(posArgs[1]),
                    Float.parseFloat(posArgs[2]), this.enemy);
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}

class CommunicatorBox extends Communicator{
    public CommunicatorBox(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){

    }
}

class CommunicatorPoint extends Communicator{
    private ConnectionManager connectionManager;

    private GameState gameState;

    public CommunicatorPoint(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        try {
            String point = this.connectionManager.receive("point");
            String[] points = point.split(":", 2);
            this.gameState.putPoint(Integer.parseInt(points[0]), Integer.parseInt(points[1]));
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}

class CommunicatorGame extends Communicator{
    private ConnectionManager connectionManager;

    private GameState gameState;

    public CommunicatorGame(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        try {
            String point = this.connectionManager.receive("game");
            if(Objects.equals(point, "lost")){
                this.gameState.setGameStatus('L');
            } else if(Objects.equals(point, "won")){
                this.gameState.setGameStatus('W');
            } else {
                this.gameState.setGameStatus('G');
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}