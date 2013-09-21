package ua.ho.gloryofrobots.yellowtalk.stobject;

public class STLabeledObject extends STObject {
    private static final long serialVersionUID = 1L;
    private  String mLabel;
    
    public static STLabeledObject create(String label) {
        STLabeledObject obj = new STLabeledObject(label);
        return obj;
    }
    
    STLabeledObject(String label) {
        super();
        mLabel = label;
    }
    
    public String toString() {
        return mLabel;
    }
}
