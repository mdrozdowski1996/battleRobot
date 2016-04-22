import java.util.Scanner;

public class Game {
	public static final int BOARD_SIZE = 8;
	public static final String EMPTY_ELEMENT = "_ ";
	private Robot robotA;
	private Robot robotB;
	private Turn turn;
	private boolean isEnd = false;
	private int whoWon = 1;
	//0 - a, 1 - draw, 2 - b
	
	public Game(Robot robotA, Robot robotB, Turn turn){
		this.robotA = robotA;
		this.robotB = robotB;
		this.turn = turn;
	}
	
	public String toStringResult(){
		if(this.whoWon == 0){
			return "Zwyciężył gracz A";
		}
		else if(this.whoWon == 1){
			return "Remis";
		}
		else{
			return "Zwyciężył gracz B";
		}
	}
	public boolean getIsEnd(){
		return this.isEnd;
	}
	public String getWhoWon(){
		if(this.whoWon == 2) return "B won";
		else if(this.whoWon == 0) return "A won";
		else return "draw";
	}
	public void rotateLeft(){
		if(this.turn.whichRobotHasAlreadyTurn() == 0){
			this.robotA.turnLeft();
		}
		else{
			this.robotB.turnLeft();
		}
		if(!this.turn.addTurn()){
			this.isEnd = true;
		}
	}
	
	public void rotateRight(){		
		if(this.turn.whichRobotHasAlreadyTurn() == 0){
			this.robotA.turnRight();
		}
		else{
			this.robotB.turnRight();
		}
		if(!this.turn.addTurn()){
			this.isEnd = true;
		}
	}
	
	public void goForward(){
		if(this.turn.whichRobotHasAlreadyTurn() == 0){
			this.robotA.moved();
			if(this.robotA.comparePosition(robotB) || !this.robotA.isInBoard()){
				this.robotA.movedBack();
			}
		}
		else{
			this.robotB.moved();
			if(this.robotB.comparePosition(robotA) || !this.robotB.isInBoard()){
				this.robotB.movedBack();
			}
		}
		if(!this.turn.addTurn()){
			this.isEnd = true;
		}
	}
	
	public void shot(){
		if(this.turn.whichRobotHasAlreadyTurn() == 0){
			if(this.robotB.isInRange(this.robotA.range())){
				robotB.ripped();
				this.whoWon = 0;
				this.isEnd = true;
			}
		}
		else{
			if(this.robotA.isInRange(this.robotB.range())){
				robotA.ripped();
				this.whoWon = 2;
				this.isEnd = true;
			}
		}
		if(!this.turn.addTurn()){
			this.isEnd = true;
		}
	}
	
	public void display(){
		for(int j = 1 ; j <= Game.BOARD_SIZE; j++){
			for(int i = 1; i <= Game.BOARD_SIZE; i++){
				if(this.robotA.isOnPosition(i, j)){
					System.out.print(this.robotA.toStringRobot("A "));
				}
				else if(this.robotB.isOnPosition(i, j)){
					System.out.print(this.robotB.toStringRobot("B "));
				}
				else{
					System.out.print("_ ");
				}
			}
			System.out.println();
		}
	}


	public static void main(String[] args){
		Direction direct = Direction.UP;
		Robot robotA = new Robot(1, 1, direct); 
		Robot robotB = new Robot(Game.BOARD_SIZE, Game.BOARD_SIZE, direct);
		Turn turn  = new Turn(0, 0);
		Game game = new Game(robotA, robotB, turn);
		Scanner s = new Scanner(System.in);
		
		while(!game.isEnd){
			game.display();
			String a = s.next();
			if(a.equals("<")){
				game.rotateLeft();
			}
			if(a.equals(">")){
				game.rotateRight();
			}
			if(a.equals("^")){
				game.goForward();
			}
			if(a.equals("S")){
				game.shot();
			}
			if(game.getIsEnd()){
				System.out.print(game.getWhoWon());
				break;
			}
		}
		}
	}
