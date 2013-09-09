package ua.ho.gloryofrobots.yellowtalk.stobject;

public class STImage extends STScope {
    private static final long serialVersionUID = 1L;
    
    public <T extends STObject> 
    T getAndCast(String key) {
      STSymbol symbol = STSymbol.unique(key);
      return getAndCast(symbol);
    }
}
