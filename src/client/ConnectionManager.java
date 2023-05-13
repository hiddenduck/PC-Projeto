import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.locks.ReentrantLock;

public class ConnectionManager implements AutoCloseable{
    private final BufferedReader input;
    private final PrintWriter output;
    private final Socket socket;

    private final Thread reader;

    private Map<String, Queue<String>> typeMap;

    private final ReentrantLock mapLock;

    private void fillTypeMap(){ // Encher os tipos logo no inicio, já os conhecemos todos
        this.typeMap = new HashMap<>();
        this.typeMap.put("pos", new LinkedList<>());
        this.typeMap.put("box", new LinkedList<>());
        this.typeMap.put("point", new LinkedList<>());
        this.typeMap.put("game", new LinkedList<>());
        this.typeMap.put("account", new LinkedList<>());
    }

    private ConnectionManager(BufferedReader input, PrintWriter output, Socket socket){
        this.input = input;
        this.output = output;
        this.socket = socket;
        fillTypeMap();
        this.mapLock = new ReentrantLock(); // Vou ter que trocar isto para um monitor
        this.reader = new Thread(() -> {
            String message;
            try {
                while ((message = input.readLine())!=null) {
                    Queue<String> typeQueue = this.typeMap.get(message);
                    String[] typeMessage = message.split(":", 2);
                    this.mapLock.lock();
                    try {
                        //if (typeQueue == null) this.typeMap.put(typeMessage[0], new LinkedList<String>());
                        typeQueue.add(typeMessage[1]);
                    } finally {
                        this.mapLock.unlock();
                    }
                }
            } catch (IOException e){
                e.printStackTrace();
            }
        });
    }

    public static ConnectionManager start(Socket socket) throws IOException{
        BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter output = new PrintWriter(socket.getOutputStream());

        return new ConnectionManager(input, output, socket);
    }

    public void send(String type, String message) throws IOException{
        output.println(type+":"+message);
        output.flush();
    }

    public String receive(String type) throws IOException{
        this.mapLock.lock();
        try{
            Queue<String> typeQueue = this.typeMap.get(type);
            return typeQueue.remove();
        } finally {
            this.mapLock.unlock();
        }
    }

    public void close() throws IOException, InterruptedException{
        this.socket.close();
        //this.reader.join();
        //this.input.close();

    }
}
