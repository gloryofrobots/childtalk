package ua.ho.gloryofrobots.yellowtalk.stobject;

import ua.ho.gloryofrobots.yellowtalk.Universe;
import ua.ho.gloryofrobots.yellowtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.yellowtalk.scheduler.Routine;
import ua.ho.gloryofrobots.yellowtalk.stobject.classprovider.BindingClassProvider;

public class STMetaclass extends STClass {
   
    private static final long serialVersionUID = 1L;
    STClass mInstanceClass;
    
    protected STMetaclass() {
        STMethod creator = new STMethod();
        creator.setPrimitiveName(STSymbol.unique("Behaviour_createSubclass"));
        creator.setArguments(new String[] { "name", "instVarNames",
                "classVarNames" });
        mScope.put(STSymbol
                .unique("subclass:instanceVariableNames:classVariableNames:"),
                creator);

        setPrimitive("Behaviour_createSubclass", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                 STMetaclass.createClassInRuntime(this, routine, receiver, stack);
                 return Universe.objects().NIL;
            }
        });
    }
    public STClass createSubclassOf(STSymbol className, STClass superclass) {
        mInstanceClass = new STClass();
        
        STClass superMeta = superclass.getSTClass();
        //link MetaClasses for static scope
        this.setSuperClass(superMeta);
        
        mInstanceClass.setSuperClass(superclass);
        mInstanceClass.setSTClass(this);
        mInstanceClass.setName(className);
        superclass.addSubclass(mInstanceClass);
        mScope.put(STSymbol.unique("instanceClass"), mInstanceClass);
        return mInstanceClass;
    }

    public static STObject createClassInRuntime(STPrimitive primitive,
            Routine routine, STObject receiver, STStack stack) {
        STClass superclass = receiver.castToSubclass();
        STSymbol className = routine.getArgument(0).castToSubclass();
        STString instanceVars = routine.getArgument(1).castToSubclass();
        STString classVars = routine.getArgument(2).castToSubclass();

        STMetaclass metaclass = STMetaclass.create();
        STClass klass = metaclass.createSubclassOf(className, superclass);

        STArray instanceVarsArray = instanceVars.splitToSymbols(" ");
        if (instanceVarsArray != null) {
            try {
                klass.addInstanceVariables(instanceVarsArray);
            } catch (DuplicateVariableException e) {
                primitive.primitiveError(routine, "Duplicate variable %s",
                        e.toString());
            }
        }
        
        STArray classVarsArray = classVars.splitToSymbols(" ");
        if (classVarsArray != null) {
            try {
                klass.addClassVariables(classVarsArray);
            } catch (DuplicateVariableException e) {
                primitive.primitiveError(routine, "Duplicate variable %s",
                        e.toString());
            }
        }
        Universe.image().put(className, klass);
        return klass;
    }
    
    
    public static STMetaclass create() {
        STMetaclass meta = new STMetaclass();

        meta.setClassProvider(new BindingClassProvider(meta) {
            @Override
            protected STClass _getSTClass() {
                return Universe.classes().Metaclass;
            }
        });

        return meta;
    }
    
    public String toString() {
        return "<STMetaclass" + ((mInstanceClass != null) ? " for " + mInstanceClass.toString() : "") + ">";
    }
}
