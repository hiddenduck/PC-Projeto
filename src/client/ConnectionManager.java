import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.*;

public class ConnectionManager implements AutoCloseable{
    BufferedReader input;
    PrintWriter output;
    Socket socket;

    private ConnectionManager(BufferedReader input, PrintWriter output, Socket socket){
        this.input = input;
        this.output = output;
        this.socket = socket;
    }

    public static ConnectionManager start(Socket socket) throws IOException{
        BufferedReader input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter output = new PrintWriter(socket.getOutputStream());
        ConnectionManager connectionManager = new ConnectionManager(input, output, socket);
        
        return connectionManager;
    }

    public void send(String type, String message) throws IOException{
        output.println(type+":"+message);
        output.flush();
    }

    public String receive(String type) throws IOException{ // Falta tratar tipos
        return input.readLine();
    }

    public void close() throws IOException{
        socket.close();
    }
}
