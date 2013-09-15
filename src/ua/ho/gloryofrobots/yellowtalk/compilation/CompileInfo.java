package ua.ho.gloryofrobots.yellowtalk.compilation;
import ua.ho.gloryofrobots.yellowtalk.IntArray;

public class CompileInfo {
    IntArray mPositions;
    char[] mData;
    
    CompileInfo() {
    }
    
    public void catchDataFromStream(ProgramTextStreamInterface stream) {
        
    }
    
    public void setPosition(int index, int position) {
        mPositions.set(index, position);
    }
    
    public int getPosition(int index) {
        return mPositions.get(index);
    }
    
}
