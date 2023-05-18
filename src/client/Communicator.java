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
    private final boolean enemy;

    public CommunicatorPos(ConnectionManager connectionManager, GameState gameState, boolean enemy){
        super(connectionManager, gameState);
        this.enemy = enemy;
    }

    public void run(){
        try {
            String pos = this.connectionManager.receive("pos");
            String[] posArgs = pos.split(":", 3);
            this.gameState.lrw.readLock().lock();
            try {
                this.gameState.putPos(Float.parseFloat(posArgs[0]), Float.parseFloat(posArgs[1]),
                        Float.parseFloat(posArgs[2]), this.enemy);
            } finally {
                this.gameState.lrw.readLock().unlock();
            }
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
        try {
            String box = this.connectionManager.receive("box");
            String[] args = box.split(":", 4);
            this.gameState.lrw.readLock().lock();
            try {
                if (Objects.equals(args[0], "+"))
                    this.gameState.putBox(new Triple(Float.parseFloat(args[1]), Float.parseFloat(args[2])
                            , args[3].charAt(0)));
                else
                    this.gameState.removeBox(new Triple(Float.parseFloat(args[1]), Float.parseFloat(args[2])
                            , args[3].charAt(0)));
            } finally {
                this.gameState.lrw.readLock().unlock();
            }
        } catch (Exception e){
            e.printStackTrace();
        }
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
            this.gameState.lrw.readLock().lock();
            try {
                this.gameState.putPoint(Integer.parseInt(points[0]), Integer.parseInt(points[1]));
            } finally {
                this.gameState.lrw.readLock().unlock();
            }
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
            this.gameState.lrw.readLock().lock();
            try {
                this.gameState.setGameStatus(point.charAt(0));
            } finally {
                this.gameState.lrw.readLock().unlock();
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}