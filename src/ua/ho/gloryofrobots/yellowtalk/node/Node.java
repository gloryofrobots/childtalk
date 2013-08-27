package ua.ho.gloryofrobots.yellowtalk.node;

import java.util.List;

public abstract class Node {
    protected class StringWriter {
        protected int getLevel() {
            return mLevel;
        }

        protected void setLevel(int mLevel) {
            this.mLevel = mLevel;
        }
        
        protected String getLevelSpaces(int nodeLevel) {
            String spaces = new String();
            
            int level = nodeLevel;
            while(level > 0) {
                spaces += "    ";
                level--;
            }
            int padding = mPadding;
            while(padding > 0) {
                spaces += " ";
                padding--;
            }
            return spaces;
        }
        
        protected String getIndentedString(String data) {
            return getLevelSpaces(mLevel) + data;
        }
           
        public void write(String format,  Object... args) {
            write(String.format(format, args));
        }
        
        public void writeln(String format,  Object... args) {
            write(String.format(format, args) + "\n" );
        }
        
        public void write(String data) {
            mData += getIndentedString(data);
        }
        
        public void write(StringWriter writer) {
            write(writer.toString());
        }
        
        public void increaseLevel() {
            mLevel++;
        }
        
        public void decreaseLevel() {
            mLevel--;
        }
        
        public String toString() {
            return mData;
        }
        
        public void setPadding(int padding) {
            mPadding = padding;
        }
        
        private int mLevel = 0;
        int mPadding = 0;
        private String mData = new String();
        
       
    }
   
   //PRINTING ////////////////////////////////////////////////////////////// 
    public String toString() {
        StringWriter writer = new StringWriter();
        writeRepresentation(writer);
        //return "{" + getClass().getSimpleName() + " " + writer.toString() + "}";
        return writer.toString();
    }
    
   
    abstract void writeRepresentation(StringWriter writer);
    
    
  //PRINTING ////////////////////////////////////////////////////////////// 
}

