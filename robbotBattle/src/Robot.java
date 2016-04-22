
public class Robot {
	private int x;
	private int y;
	private Direction myDirection;
	private boolean isAlive = true;
	
	public Robot(int x, int y, Direction myDirection){
		this.x = x;
		this.y = y;
		this.myDirection = myDirection;
	}
	
	public void turnRight(){
		this.myDirection = myDirection.next();
	}
	
	public void turnLeft(){
		this.myDirection = myDirection.back();
	}
	
	public boolean isOnPosition(int x, int y){
		return this.x == x && this.y == y;
	}
	
	public boolean comparePosition(Robot robot){
		return this.x == robot.returnX() && this.y == robot.returnY();
	}
	
	public int returnX(){
		return this.x;
	}
	
	public int returnY(){
		return this.y;
	}
	
	public boolean returnIsAlive(){
		return this.isAlive;
	}
	public boolean isInBoard(){
		return 1<=this.x && this.x <= Game.BOARD_SIZE && 1 <= this.y && this.y <= Game.BOARD_SIZE; 
	}
	public void moved(){
		switch(this.myDirection){
		case UP:
			 this.y--;
			 break;
		case DOWN:
			 this.y++;
			 break;
		case RIGHT:
			 this.x++;
			 break;
		case LEFT:
			 this.x--;
			 break;
		}
	}
	public void movedBack(){
		switch(this.myDirection){
		case UP:
			 this.y++;
			 break;
		case DOWN:
			 this.y--;
			 break;
		case RIGHT:
			 this.x--;
			 break;
		case LEFT:
			 this.x++;
			 break;
		}
	}
	
	public void ripped(){
		this.isAlive = false;
	}
	
	public int[] range(){
		int[] coordinates = new int[4];
		switch(this.myDirection){
			case DOWN:
				coordinates[0] = this.x;
				coordinates[1] = this.x;
				coordinates[2] = this.y;
				coordinates[3] = Game.BOARD_SIZE ;
				break;
			case UP:
				coordinates[0] = this.x;
				coordinates[1] = this.x;
				coordinates[2] = 1;
				coordinates[3] = this.y;
				break;
			case LEFT:
				coordinates[0] = 1;
				coordinates[1] = this.x;
				coordinates[2] = this.y;
				coordinates[3] = this.y;
				break;
			case RIGHT:
				coordinates[0] = this.x;
				coordinates[1] = Game.BOARD_SIZE ;
				coordinates[2] = this.y;
				coordinates[3] = this.y;
				break;
		}
		return coordinates;
	}
	public String toStringRobot(String name){
		return this.myDirection.getMyChar() + name; 
	}
	public boolean isInRange(int[] coordinates){
		return this.x <= coordinates[1] && this.x >= coordinates[0] && this.y >= coordinates[2] && this.y <= coordinates[3];
	}
}


