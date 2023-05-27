import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

public class ConnectionManager implements AutoCloseable{
    private final BufferedReader input;
    private final PrintWriter output;
    private final Socket socket;

    private final Thread reader;

    private Map<String, Queue<String>> typeMap;

    private void fillTypeMap(){ // Encher os tipos logo no inicio, já os conhecemos todos
        this.typeMap = new HashMap<>();
        this.typeMap.put("pos", new LinkedList<>());
        this.typeMap.put("posE", new LinkedList<>());
        this.typeMap.put("box", new LinkedList<>());
        this.typeMap.put("points", new LinkedList<>());
        this.typeMap.put("game", new LinkedList<>());
        this.typeMap.put("register", new LinkedList<>());
        this.typeMap.put("login", new LinkedList<>());
        this.typeMap.put("logout", new LinkedList<>());
        this.typeMap.put("ready", new LinkedList<>());
        this.typeMap.put("close", new LinkedList<>());
        this.typeMap.put("top", new LinkedList<>());
    }

    private ConnectionManager(BufferedReader input, PrintWriter output, Socket socket){
        this.input = input;
        this.output = output;
        this.socket = socket;
        fillTypeMap();
        this.reader = new Thread(() -> {
            String message;
            try {
                while ((message = input.readLine())!=null) {
                    String[] typeMessage = message.split(":", 2);
                    Queue<String> typeQueue = this.typeMap.get(typeMessage[0]);
                    synchronized (typeQueue) {
                        typeQueue.add(typeMessage[1]);
                        typeQueue.notify();
                    }
                }
            } catch (IOException e){
            }
        });
        this.reader.start();
    }

    public static ConnectionManager start(Socket socket) throws IOException{
        BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter output = new PrintWriter(socket.getOutputStream());
        return new ConnectionManager(input, output, socket);
    }

    public void send(String type, String message) throws IOException{
        output.printf("%s:%s\n", type,message);
        output.flush();
    }

    public String receive(String type) throws IOException, InterruptedException{
        Queue<String> typeQueue = this.typeMap.get(type);
        synchronized (typeQueue) {
            while (typeQueue.isEmpty()) {
                typeQueue.wait();
            }
        }

        return typeQueue.remove();
    }

    public void close() throws IOException, InterruptedException{
        for(Queue<String> queue: typeMap.values()){
            queue.add(null);
        }
        this.socket.close();
        this.input.close();
        this.output.close();
        this.reader.join();
    }
}
