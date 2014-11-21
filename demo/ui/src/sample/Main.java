package sample;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.text.Font;
import javafx.scene.text.TextAlignment;
import javafx.stage.Stage;
import javafx.stage.Window;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

public class Main extends Application {

	public static int port;
	public static int type;
	public static int num = 100;
	public Scene scene;
	public static Image red;
	public static Image green;
	public static ArrayList<Label> roomLabelList;

    @Override
    public void start(Stage primaryStage) throws Exception {
		this.scene = new Scene(new Group());
		Pane pane = new Pane();
		this.red = new Image(getClass().getResourceAsStream("/red.png"));
		this.green = new Image(getClass().getResourceAsStream("/green.png"));

		Label title = new Label();
		title.setFont(new Font(24));
		title.setPrefWidth(600);
		title.setLayoutX(50);
		title.setLayoutY(32);

		Label legendGreen = new Label();
		ImageView legendImgGreen = new ImageView(this.green);
		legendImgGreen.setFitWidth(30);
		legendImgGreen.setFitHeight(30);
		legendGreen.setGraphic(legendImgGreen);
		legendGreen.setLayoutX(24);
		legendGreen.setLayoutY(82);
		pane.getChildren().add(legendGreen);

		Label legendRed = new Label();
		ImageView legendImgRed = new ImageView(this.red);
		legendImgRed.setFitWidth(30);
		legendImgRed.setFitHeight(30);
		legendRed.setGraphic(legendImgRed);
		legendRed.setLayoutX(24);
		legendRed.setLayoutY(122);
		pane.getChildren().add(legendRed);

		Label legendTextGreen = new Label();
		legendTextGreen.setLayoutX(80);
		legendTextGreen.setLayoutY(88);
		pane.getChildren().add(legendTextGreen);

		Label legendTextRed = new Label();
		legendTextRed.setLayoutX(80);
		legendTextRed.setLayoutY(128);
		pane.getChildren().add(legendTextRed);

		if (Main.type == 1) {
			title.setText("Bảng giám sát điều kiện phòng");
			legendTextGreen.setText("Điều kiện tốt");
			legendTextRed.setText("Điều kiện xấu");
		} else {
			title.setText("Bảng giám sát sự hiện diện phòng");
			legendTextGreen.setText("Không có người");
			legendTextRed.setText("Có người");
		}

		title.setAlignment(Pos.BASELINE_CENTER);
		pane.getChildren().add(title);
		int padX = 70;
		int padY = 170;

		this.roomLabelList = new ArrayList<Label>();
		for (int i = 0; i < Main.num; i++) {
			if (i%10 == 0) {
				Label number = new Label();
				number.setText(new Integer(i+1).toString());
				number.setFont(new Font(16));
				number.setLayoutX(30);
				number.setLayoutY(padY+16+(i/10)*60);
				pane.getChildren().add(number);
			}
			ImageView iv = new ImageView(this.green);
			iv.setFitHeight(60);
			iv.setFitWidth(60);
			Label label = new Label();
			label.setGraphic(iv);
			roomLabelList.add(label);
			pane.getChildren().add(label);
			label.setLayoutX(padX+(i%10)*60);
			label.setLayoutY(padY+(i/10)*60);
		}

		((Group)scene.getRoot()).getChildren().add(pane);
		primaryStage.setHeight(800);
		primaryStage.setWidth(700);
		primaryStage.setScene(scene);
		primaryStage.setResizable(false);
        primaryStage.show();

		initiateListen();
	}

	static void initiateListen() {
//		while (true) ;
		Listener listener = new Listener();
		listener.start();
	}

    public static void main(String[] args) {
		if (args.length < 2) {
			System.out.println("Not enough parameters");
			return;
		}

		Main.port = Integer.parseInt(args[0]);
		Main.type = Integer.parseInt(args[1]);

		if (args.length > 2)
			Main.num = Integer.parseInt(args[2]);

        launch(args);
	}
}
