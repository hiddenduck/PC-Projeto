import java.io.IOException;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

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
                this.gameState.lrw.readLock().lock();
                try {
                    int i,j=0;
                    float[] coords = new float[3];
                    StringBuilder temp = new StringBuilder();
                    boolean minus = true;
                    if(box.charAt(0)=='+'){
                        for(i=1; i<box.length() && box.charAt(i)!='-'; i++){
                            if(box.charAt(i)!=':'){
                                temp.append(box.charAt(i));
                            }else{
                                coords[j++] = Float.parseFloat(temp.toString());
                                temp = new StringBuilder();
                            }
                        }
                        j=0;
                        this.gameState.putBox(new Triple(coords[0], coords[1], coords[2]));
                        if(i==box.length()) minus = false;
                    } else i = 1;
                    if(minus) {
                        Set<Triple> boxes = new HashSet<>();
                        for (; i < box.length(); i++) {
                            if (box.charAt(i) != ':') {
                                temp.append(box.charAt(i));
                            } else {
                                if(j==2){
                                    boxes.add(new Triple(coords[0], coords[1], coords[2]));
                                    j=0;
                                } else {
                                    coords[j++] = Float.parseFloat(temp.toString());
                                    temp = new StringBuilder();
                                }
                            }
                        }
                        this.gameState.removeBoxes(boxes);
                    }

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
                    this.gameState.putPoint(points[0], points[1]);
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