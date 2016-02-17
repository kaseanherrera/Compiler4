package crux;
import java.awt.List;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

public class Scanner implements Iterable<Token> {
	//my information 
	public static String studentName = "Kasean Herrera";
	public static String studentID = "33531582";
	public static String uciNetID = "kaseanh";
	

	
	boolean go;
	String charMatches = "([-()@'~%<{}>=:,/.=\\\"!/-;\\[\\]+*][=:]?)|(?:\\d+)\\.?\\d*|[a-zA-Z]+|" ; //string that match basic characters
	String words = "[a-zA-Z\\_]+";
	String ignoreChars = "\n|\\s";
	public StringBuilder masterStr;
	private String lastMatch; //tracks the last match that we had
	boolean updateLineNumber;
	private int lineNum;  // current line count
	private int charPos;  // character offset for current line
	private int nextChar; // contains the next char (-1 == EOF)
	private String nextCharString; //char form of nect char
	private Reader input;
	private int bufferStringPosition;
	private int lastMarkedPosition;
	
	//initialize the Scanner
	Scanner(Reader reader)
	{
	
		this.lineNum  = 1;
		this.input = reader;
		updateLineNumber = false;
		go = true;
		masterStr = new StringBuilder();
		this.lastMatch = "";
		lastMarkedPosition = 0;
		bufferStringPosition = 0;
	}	
	
	// OPTIONAL: helper function for reading a single char from input
	//           can be used to catch and handle any IOExceptions,
	//           advance the charPos or lineNum, etc.
	private int readChar() {
		//if we are at the end of buffer , get char from file
		if(bufferStringPosition == masterStr.toString().length()){
			try{
				//get the next char
				nextChar = input.read();
				nextCharString = Character.toString((char)nextChar);
				//add the next char to the string if it is not in list of ignore characters 
				if(!nextCharString.matches(ignoreChars))
					masterStr.append(nextCharString);
			}catch(IOException ex){
				return -1;
			}
		}
		else{
			//get char from the buffer
			nextChar = masterStr.toString().charAt(bufferStringPosition);
			nextCharString = Character.toString((char)nextChar);
		}
		
		if(nextChar == '\n'){
			updateLineNumber = true;  
		}else{
			//add to the current Pos 
		//	charPos++;
			if(!nextCharString.matches(ignoreChars))
				bufferStringPosition++;
		}
		
		if(updateLineNumber){
			lineNum++;
			charPos = 0;
			updateLineNumber = false;
		}
		return nextChar;
		
	}
		
	/* Invariants:
	 *  1. call assumes that nextChar is already holding an unread character
	 *  2. return leaves nextChar containing an untokenized character
	 */
	public Token next()
	{		
		//get go 
		go = true;
		while(go){
			//get the next character 
			readChar();
			//check if the next char is one of the 'basic kinds' stop and tokenize
			
			if((masterStr.toString()).matches(charMatches) || masterStr.toString().matches(words)){
				//set the last match to the currentString
				lastMatch = masterStr.toString();
				//set the last match position 
				lastMarkedPosition = bufferStringPosition;
			}
			
			//if it matches any of the ignore characters, move on 
			else if((nextCharString).matches(ignoreChars)){
				//readChar();
			}
			
			else if(this.masterStr.toString().matches("//")){
				//move the scanner to the new line
				try {
					this.readToNewLine();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			//does not match anything
			else{
				reset();
			}
			//if next char is a space then we break 
			if(nextCharString.equals(" ")){
				reset();
			}
			
			/*if(nextChar == -1){
				
				return new Token(this.lineNum, this.charPos);
			} */
		}

		//create a token of the last match
		if(lastMatch == ""){
			//return new Token("EOF", this.lineNum, this.charPos);
		}
		
		Token T = new Token(lastMatch, this.lineNum, this.charPos - lastMatch.length() + 1);
		//reset last matched
		lastMatch = "";
		return T;
	}

	private void readToNewLine() throws IOException {
		int out = input.read();
		while(out != 10){
			out = input.read();
			//System.out.println(out);
		} 
		
		this.lastMatch = "";
		this.lineNum = lineNum + 1;
		this.charPos = 1;
		bufferStringPosition = 0;
		lastMarkedPosition = 0;	
		masterStr = new StringBuilder();
		
	}

	//resets the pointer and all variables when we a tokenize 
	public void reset(){
		if(masterStr.length() > 0)
			go = false;
		//set char postion back to last place we match 
		charPos = this.lastMarkedPosition;
		//delete the matched part of the master string 
		masterStr = new StringBuilder(masterStr.toString().substring(lastMarkedPosition, masterStr.length()));
		//move the buffer back 
		bufferStringPosition = 0;
		lastMarkedPosition = 0;	
	}
	
	//must have for this class 
	@Override
	public Iterator<Token> iterator() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
