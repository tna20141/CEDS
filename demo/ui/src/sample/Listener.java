package sample;

import javafx.application.Platform;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.management.PlatformLoggingMXBean;
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
					updateImage(roomLabel, Main.green);
				else
					updateImage(roomLabel, Main.red);

				in.close();
				client.close();
			}
		} catch (IOException e) {
			System.out.println(e.getMessage());
		}
	}

	public void updateImage(Label label, Image img) {
		final Label innerLabel = label;
		final Image innerImg = img;
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				((ImageView) innerLabel.getGraphic()).setImage(innerImg);
			}
		});
	}
}
