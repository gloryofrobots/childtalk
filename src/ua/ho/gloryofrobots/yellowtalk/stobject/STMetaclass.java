package ua.ho.gloryofrobots.yellowtalk.stobject;

public class STMetaclass extends STClass {
   
    private static final long serialVersionUID = 1L;

    public STClass createSubclassOf(STSymbol className, STClass superclass) {
        STClass klass = new STClass();
        
        klass.setSuperClass(superclass);
        klass.setSTClass(this);
        klass.setName(className);
        superclass.addSubclass(klass);
        
        return klass;
    }
}
