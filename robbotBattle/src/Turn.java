
public class Turn {
	public static int limitTurns = 40;
	private int numberOfTurn;
	private int whichRobot;
	//0 - aRobot turn, 1 - bRobot turn
	
	public Turn(int numberOfTurn, int whichRobot){
		this.numberOfTurn = numberOfTurn;
		this.whichRobot = whichRobot;
	}
	
	public int whichRobotHasAlreadyTurn(){
		return whichRobot;
	}
	public boolean addTurn(){
		this.numberOfTurn++;
		this.whichRobot = 1 - this.whichRobot;
		if(this.numberOfTurn > Turn.limitTurns ){
			return false;
		}
		else return true;
	}
	
}
