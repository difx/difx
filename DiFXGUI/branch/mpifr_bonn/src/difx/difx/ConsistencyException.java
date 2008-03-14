package difx;

public class ConsistencyException extends Exception {

	private String errorMessage;
	
	public ConsistencyException() {
		errorMessage = "configuration consistency violated - cause unknown";
	}

	public ConsistencyException(String reason) {
		super(reason);
		errorMessage = "configuration consistency violated - " + reason;
	}

	public String getMessage() {
		return errorMessage;
	}
}
