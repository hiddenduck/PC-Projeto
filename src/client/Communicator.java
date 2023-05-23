import java.io.IOException;
import java.util.Objects;

public class Communicator extends Thread{
    public ConnectionManager connectionManager;
    public GameState gameState;

    public Communicator(ConnectionManager connectionManager, GameState gameState){
        this.connectionManager = connectionManager;
        this.gameState = gameState;
    }
}

class CommunicatorPos extends Communicator {
    private final String enemy;

    public CommunicatorPos(ConnectionManager connectionManager, GameState gameState, String enemy) {
        super(connectionManager, gameState);
        this.enemy = enemy;
    }

    public void run() {
        String pos = null;
        do {
            try {
                pos = this.connectionManager.receive("pos" + this.enemy);
                String[] posArgs = pos.split(":", 3);
                this.gameState.lrw.readLock().lock();
                try {
                    this.gameState.putPos(Float.parseFloat(posArgs[0]), Float.parseFloat(posArgs[1]),
                            Float.parseFloat(posArgs[2]), Objects.equals(this.enemy, "E"));
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            } catch (IOException | InterruptedException e) {
                e.printStackTrace();
            }
        } while (pos != null);
    }
}

class CommunicatorBox extends Communicator{
    public CommunicatorBox(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        String box = null;
        do {
            try {
                box = this.connectionManager.receive("box");
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
            } catch (IOException | InterruptedException e) {
                e.printStackTrace();
            }
        } while (box != null);
    }
}

class CommunicatorPoint extends Communicator{

    public CommunicatorPoint(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        String point = null;
        do {
            try {
                point = this.connectionManager.receive("points");
                String[] points = point.split(":", 2);
                this.gameState.lrw.readLock().lock();
                try {
                    this.gameState.putPoint(Integer.parseInt(points[0]), Integer.parseInt(points[1]));
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        } while(point!=null);
    }
}

class CommunicatorGame extends Communicator{
    public CommunicatorGame(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        String game = null;
        do {
            try {
                game = this.connectionManager.receive("game");
                this.gameState.lrw.readLock().lock();
                try {
                    this.gameState.setGameStatus(game);
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            } catch (IOException | InterruptedException e) {
                e.printStackTrace();
            }
        } while (game != null);
    }
}