import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class ConnectionManager implements AutoCloseable{
    private final BufferedReader input;
    private final PrintWriter output;
    private final Socket socket;

    private final Thread reader;

    private final Lock mapLock;
    private class TypeQueue{
        Queue<String> queue = new LinkedList<String>();

        Condition c = mapLock.newCondition();
    }

    private Map<String, TypeQueue> typeMap;

    private void fillTypeMap(){ // Encher os tipos logo no inicio, já os conhecemos todos
        this.typeMap = new HashMap<>();
        this.typeMap.put("pos", new TypeQueue());
        this.typeMap.put("box", new TypeQueue());
        this.typeMap.put("point", new TypeQueue());
        this.typeMap.put("game", new TypeQueue());
        this.typeMap.put("account", new TypeQueue());
    }

    private ConnectionManager(BufferedReader input, PrintWriter output, Socket socket){
        this.input = input;
        this.output = output;
        this.socket = socket;
        fillTypeMap();
        this.mapLock = new ReentrantLock();
        this.reader = new Thread(() -> {
            String message;
            try {
                while ((message = input.readLine())!=null) {
                    TypeQueue typeQueue = this.typeMap.get(message);
                    String[] typeMessage = message.split(":", 2);
                    this.mapLock.lock();
                    try {
                        typeQueue.queue.add(typeMessage[1]);
                        typeQueue.c.signal(); // Só vai haver uma thread de cada tipo logo não é preciso signalAll
                    } finally {
                        this.mapLock.unlock();
                    }
                }
            } catch (IOException e){
                e.printStackTrace();
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
        output.println(type+":"+message);
        output.flush();
    }

    public String receive(String type) throws IOException, InterruptedException{
        this.mapLock.lock();
        try{
            TypeQueue typeQueue = this.typeMap.get(type);
            while(typeQueue.queue.isEmpty())
                typeQueue.c.await();
            return typeQueue.queue.remove();
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
