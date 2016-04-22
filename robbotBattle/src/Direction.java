

public enum Direction {
	UP('^'), RIGHT('>'), DOWN('v'), LEFT('<');
	
	private static Direction[] vals = values();
	
	private char myChar;
	
	private Direction (char currentChar){
		this.myChar = currentChar;
	}
	
	public Direction next(){
		int currentIndex = this.ordinal();
		int destinationIndex = (currentIndex+1)%(this.vals.length);
		return this.vals[destinationIndex];
	}
	
	public Direction back(){
		int currentIndex = this.ordinal() + 4;
		int destinationIndex = (currentIndex - 1) % (this.vals.length);
		return this.vals[destinationIndex];
	}
	public String getMyChar(){
		return this.myChar+"";
	}
	
	
}
