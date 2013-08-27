package ua.ho.gloryofrobots.yellowtalk;

public interface ProgramTextStreamInterface{
    final int INVALID_CHARACTER = -1;
    
    public class PositionInfo {
        public int line;
        public int column;
    }
    
    public boolean goForward();
    public boolean goBackward(int count);
    
    public char getCurrentChar();
    public int getPosition();
    public PositionInfo getPositionInfo(int position);
}