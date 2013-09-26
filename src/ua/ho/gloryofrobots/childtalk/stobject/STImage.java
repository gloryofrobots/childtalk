package ua.ho.gloryofrobots.childtalk.stobject;

public class STImage extends STScope {
    private static final long serialVersionUID = 1L;
    
    public <T extends STObject> 
    T getAndCast(String key) {
      STSymbol symbol = STSymbol.create(key);
      return getAndCast(symbol);
    }
    
    @Override
    public String toString() {
        /*String result = "";
        for(STObject obj : mData) {
            if(obj == this) {
                continue;
            }
            result += obj.toString() + "\n";
        }
        
        return result;*/
        return "Image";
    }
}
