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
        try{
            while((pos = this.connectionManager.receive("pos" + this.enemy))!=null) {
                String[] posArgs = pos.split(":", 3);
                this.gameState.lrw.readLock().lock();
                try {
                    this.gameState.putPos(Float.parseFloat(posArgs[0]), Float.parseFloat(posArgs[1]),
                            Float.parseFloat(posArgs[2]), Objects.equals(this.enemy, "E"));
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            }
        } catch (IOException | InterruptedException e){
            e.printStackTrace();
        }
    }
}

class CommunicatorBox extends Communicator{
    public CommunicatorBox(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }
    public void run(){
        String box = null;
        try {
            while((box=this.connectionManager.receive("box"))!=null){
                boolean remove=false, add=false;
                String[] addRemove = box.split("_");
                String[] addBoxes = new String[0], removeBoxes = new String[0];
                if(box.charAt(0)!='_') {
                    addBoxes = addRemove[0].split(",");
                    add = true;
                }
                if(addRemove.length==2) {
                    removeBoxes = addRemove[1].split(",");
                    remove = true;
                }
                String[] boxN;
                this.gameState.lrw.readLock().lock();
                try {
                    if(add) {
                        for (String addBox : addBoxes) {
                            boxN = addBox.split(":");
                            this.gameState.putBox(new Triple(Float.parseFloat(boxN[0]), Float.parseFloat(boxN[1]), boxN[2].charAt(0)));
                        }
                    }
                    if(remove) {
                        for (String removeBox : removeBoxes) {
                            boxN = removeBox.split(":");
                            this.gameState.removeBox(new Triple(Float.parseFloat(boxN[0]), Float.parseFloat(boxN[1]), boxN[2].charAt(0)));
                        }
                    }
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            }
        } catch (java.io.IOException|java.lang.InterruptedException e){
            e.printStackTrace();
        }
    }
    /*
    public void run(){
        String box = null;
        try{
            while((box = this.connectionManager.receive("box"))!=null){
                System.out.println(box);
                boolean minus = true, plus = false;
                int i = 0, j = 0;
                StringBuilder temp = new StringBuilder();
                String[] coords1 = new String[3];
                if(box.charAt(0)=='+'){
                    plus = true;
                    for(i=2; i<box.length() && box.charAt(i)!='-'; i++){
                        if(box.charAt(i)!=':'){
                            temp.append(box.charAt(i));
                        }else{
                            coords1[j++] = temp.toString();
                            temp = new StringBuilder();
                        }
                    }
                    if(i==box.length()){
                        coords1[j] = temp.toString();
                        temp = new StringBuilder();
                        minus = false;
                    }
                    j=0;
                }
                Set<Triple> boxes = new HashSet<>();
                if(minus) {
                    String[] coords = new String[3];
                    //saltar o "-:"
                    for (i+=2; i < box.length(); i++) {
                        if (box.charAt(i) != ':') {
                            temp.append(box.charAt(i));
                        } else {
                            coords[j++] = temp.toString();
                            temp = new StringBuilder();

                            if(j==3) {
                                boxes.add(new Triple(Float.parseFloat(coords[0]), Float.parseFloat(coords[1]), coords[2].charAt(0)));
                                j=0;
                            }
                        }
                    }
                    coords[j++] = temp.toString();
                    temp = new StringBuilder();
                    //este teste é irrelevante porque ele deve ser sempre j==2
                    if(j==3) {
                        boxes.add(new Triple(Float.parseFloat(coords[0]), Float.parseFloat(coords[1]), coords[2].charAt(0)));
                        j=0;
                    }

                }
                this.gameState.lrw.readLock().lock();
                try{
                    if(minus){
                        this.gameState.removeBoxes(boxes);
                    }
                    if(plus){
                        this.gameState.putBox(new Triple(Float.parseFloat(coords1[0]), Float.parseFloat(coords1[1]), coords1[2].charAt(0)));
                    }
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            }
        } catch (IOException | InterruptedException e){
            e.printStackTrace();
        }
    }
    */
}

class CommunicatorPoint extends Communicator{

    public CommunicatorPoint(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        String point = null;
            try {
                while ((point = this.connectionManager.receive("points")) != null){
                    String[] points = point.split(":", 2);
                    this.gameState.lrw.readLock().lock();
                    try {
                        this.gameState.putPoint(points[0], points[1]);
                    } finally {
                        this.gameState.lrw.readLock().unlock();
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
    }
}

class CommunicatorGame extends Communicator{
    public CommunicatorGame(ConnectionManager connectionManager, GameState gameState){
        super(connectionManager, gameState);
    }

    public void run(){
        String game = null;
        try {
            while ((game = this.connectionManager.receive("game"))!=null){
                this.gameState.lrw.readLock().lock();
                try {
                    this.gameState.setGameStatus(game);
                } finally {
                    this.gameState.lrw.readLock().unlock();
                }
            }
        } catch (IOException | InterruptedException e){
            e.printStackTrace();
        }
    }
}