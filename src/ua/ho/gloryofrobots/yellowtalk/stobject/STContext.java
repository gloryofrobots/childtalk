package ua.ho.gloryofrobots.yellowtalk.stobject;

public class STContext extends STObject {
    private static final long serialVersionUID = 1L;
    STObject mReceiver;
    
    public STObject getReceiver() {
        return mReceiver;
    }
    
    public void setReceiver(STObject mReceiver) {
        this.mReceiver = mReceiver;
    }
    
    public void assign(STObject varName, STObject value) {
        mScope.assign(varName, value);
    }

    public void pushScope(STScope scope) {
        scope.append(mScope);
        setScope(scope);
    }
}
