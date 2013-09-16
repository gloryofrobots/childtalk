package ua.ho.gloryofrobots.yellowtalk.compilation;
import ua.ho.gloryofrobots.yellowtalk.IntArray;

public class CompileInfo {
    public static final int UNKNOWN_POSITION = -1; 
    IntArray mPositions;
    char[] mData;
    private int mFirstIndex;
    private int mLastIndex;
    private boolean mIsInitialised;
    
    public CompileInfo() {
        mPositions = new IntArray();
    }
    
    public void intitialise(int begin, int end, ProgramTextStreamInterface stream) {
        mIsInitialised = true;
        mFirstIndex = begin;
        mLastIndex = end;
        mData = stream.getRange(begin, end);
    }
    
    public void setPosition(int index, int position) {
        int realPosition = position - mFirstIndex; 
        mPositions.set(index, realPosition);
    }
    
    public int getPosition(int index) {
        while(true) {
            int position = mPositions.get(index);
            if(position > 0) {
                return position;
            }
            
            index++;
        }
        
    }

    public String getCodeLine(int index) {
        //DEBUG ONLY
        if(mIsInitialised == false) {
            throw new RuntimeException();
        }
        String post = "";
        int position = getPosition(index);
        int size = mData.length;
        for(int i = position; i < size; i++) {
            char ch = mData[i];
            if(ch == '\n') {
                break;
            }
            if(Character.isWhitespace(ch)) {
                continue;
            }
            post += ch;
        }
        
        String pre = "";
        for(int i = position - 1; i >= 0; i--) {
            char ch = mData[i];
            if(ch == '\n') {
                break;
            }
            
            if(Character.isWhitespace(ch)) {
                continue;
            }
            
            pre += ch;
        }
        
        String result = new StringBuilder(pre).reverse().toString() + post;
        return result;
    }
}
