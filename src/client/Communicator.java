import java.util.Objects;

class CommunicatorPos extends Thread{
    private ConnectionManager connectionManager;

    private GameState gameState;

    private boolean enemy;

    public CommunicatorPos(ConnectionManager connectionManager){
        this.connectionManager = connectionManager;
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

class CommunicatorBox extends Thread{
    private ConnectionManager connectionManager;

    public CommunicatorBox(ConnectionManager connectionManager){
        this.connectionManager = connectionManager;
    }

    public void run(){

    }
}

class CommunicatorPoint extends Thread{
    private ConnectionManager connectionManager;

    private GameState gameState;

    public CommunicatorPoint(ConnectionManager connectionManager, GameState gameState){
        this.connectionManager = connectionManager;
        this.gameState = gameState;
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

class CommunicatorGame{
    private ConnectionManager connectionManager;

    private GameState gameState;

    public CommunicatorGame(ConnectionManager connectionManager, GameState gameState){
        this.connectionManager = connectionManager;
        this.gameState = gameState;
    }

    public void run(){
        try {
            String point = this.connectionManager.receive("game");
            if(Objects.equals(point, "lp")){
                //this.gameState.putGoldenPoint();
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}