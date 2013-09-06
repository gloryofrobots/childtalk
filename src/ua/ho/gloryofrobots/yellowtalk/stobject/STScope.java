package ua.ho.gloryofrobots.yellowtalk.stobject;

import java.util.Map;

import ua.ho.gloryofrobots.yellowtalk.DuplicateVariableException;

public class STScope extends STInternalDictionary {
    private static final long serialVersionUID = 1L;
    private STScope next = null;
    private STScope previous = null;
    
    @SuppressWarnings (value="unchecked")
    public <T extends STObject> 
    T getAndCast(STObject key) {
        STObject value = at(key);
        T obj = null;
        try {
            obj = (T) value;    
        } catch(ClassCastException e) {
            return null;
        } 
        
        return obj;
    }
    
    public void append(STScope scope) {
        next = scope;
        scope.setPrevious(this);
    }
    
    private void setPrevious(STScope prev) {
        previous = prev;
    }
    
    public void assign(STObject key, STObject value) {
         STScope scope = this;
         while(scope != null) {
             if(scope.has(key)) {
                 scope.put(key, value);
                 break;
             }
             scope = scope.next;
         }
    }
    
    public void putUnique(STObject name, STObject object)
            throws DuplicateVariableException {
        if (has(name)) {
            throw new DuplicateVariableException(name.toString());
        }
    }

    public STScope copySelf() {
        STScope clone = new STScope();

        for (Map.Entry<STObject, STObject> item : mData.entrySet()) {
            STObject key = item.getKey();
            STObject value = item.getValue();
            clone.put(key, value);
        }
        
        return clone;
    }
}
