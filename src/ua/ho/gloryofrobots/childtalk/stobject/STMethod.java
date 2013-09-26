package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.scheduler.MethodRoutine;
import ua.ho.gloryofrobots.childtalk.scheduler.PrimitiveRoutine;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STMethod extends STExecutableObject {

    private static final long serialVersionUID = 1L;

    protected STSymbol mComment;
    protected STSymbol mCategory;
    protected STSymbol mPrimitiveName = null;
    protected STSymbol mClassName;
    protected STClass mOwnerClass;
    private STSymbol mSelector;

    public void setOwnerClass(STClass owner) {
        mOwnerClass = owner;
    }

    public static STMethod create() {
        STMethod obj = new STMethod();
        obj.setClassProvider(new BindingClassProvider(obj) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Method;
            }
        });

        return obj;
    }

    public void setComment(STSymbol comment) {
        mComment = comment;
    }

    public void setCategory(STSymbol category) {
        mCategory = category;
    }

    public void setPrimitiveName(STSymbol primitive) {
        mPrimitiveName = primitive;
    }

    public STSymbol getPrimitiveName() {
        return mPrimitiveName;
    }

    public STPrimitive getPrimitive() {
        if (mOwnerClass == null) {
            //System.out.printf("Owner class null %s\n",this.toString());
            return null;
        }

        STPrimitive primitive = mOwnerClass.getPrimitive(mPrimitiveName);
        return primitive;
    }

    public boolean hasPrimitive() {
        if (mPrimitiveName == null) {
            return false;
        }

        return true;
    }

    @Override
    public Routine createRoutine() {
        Routine routine;

        if (hasPrimitive() == false) {
            routine = new MethodRoutine(this);
        } else {
            routine = new PrimitiveRoutine(this);
        }

        return routine;
    }

    public void setClassName(STSymbol name) {
        mClassName = name;
    }

    public void setSelector(STSymbol name) {
        mSelector = name;
    }

    @Override
    public String toString() {
        String result = String.format(
                "%s->%s",
                (mSelector != null) ? mSelector.toString() : "",
                (mOwnerClass != null) ? mOwnerClass.toString() : "");

        return result;
    }
}
