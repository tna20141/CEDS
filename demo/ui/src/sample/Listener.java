package sample;

import javafx.scene.control.Label;
import javafx.scene.image.ImageView;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: tna2
 * Date: 11/17/14
 * Time: 10:26 AM
 * To change this template use File | Settings | File Templates.
 */
public class Listener extends Thread {
	@Override
	public void run() {
		try {
			ServerSocket serverSock = new ServerSocket();
			serverSock.setReuseAddress(true);
			serverSock.bind(new InetSocketAddress(Main.port));
			while (true) {
				Socket client = serverSock.accept();
				InputStreamReader in = new InputStreamReader(client.getInputStream());

				int numRoom = in.read();
				int status = in.read();

				Label roomLabel = Main.roomLabelList.get(numRoom-1);
				if (status == 0)
					((ImageView)roomLabel.getGraphic()).setImage(Main.green);
				else
					((ImageView)roomLabel.getGraphic()).setImage(Main.red);
			}
		} catch (IOException e) {
			System.out.println(e.getMessage());
		}
	}
}
