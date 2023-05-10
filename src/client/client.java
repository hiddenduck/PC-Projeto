import java.net.*;

public class client {
    public static void main(String[] args) {
        if(args.length < 2)
            System.exit(1);
        
        String host = args[0];
        int port = Integer.parseInt(args[1]);
        
        try{
            Socket socket = new Socket(host, port);
            ConnectionManager cm = ConnectionManager.start(socket);
            
            // new Thread();
        } catch(Exception e){
            e.printStackTrace();
            System.exit(0);
        }
    }
}
