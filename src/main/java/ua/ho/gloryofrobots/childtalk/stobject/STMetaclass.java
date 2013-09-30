package ua.ho.gloryofrobots.childtalk.stobject;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.DuplicateVariableException;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.classprovider.BindingClassProvider;

public class STMetaclass extends STClass {
   
    private static final long serialVersionUID = 1L;
    STClass mInstanceClass;
    
    protected STMetaclass() {
        //Little tricky. When classes not loaded we need to send first message to nil
        //nil subclass: #Object instanceVariableNames: '' classVariableNames: ''.
        //So we need to fake this method in Metaclass
        
        STMethod creator = new STMethod();
        creator.setOwnerClass(this);
        creator.setPrimitiveName(STSymbol.create("Behaviour_createSubclass"));
        creator.setArguments(new String[] { "name", "instVarNames",
                "classVarNames" });
        mScope.put(STSymbol
                .create("subclass:instanceVariableNames:classVariableNames:"),
                creator);

        setPrimitive("Behaviour_createSubclass", new STPrimitive() {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                 STMetaclass.createClassInRuntime(this, routine, receiver, stack);
                 return ImageSuite.image().objects().NIL;
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
        mScope.put(STSymbol.create("instanceClass"), mInstanceClass);
        
        STSymbol initMethodName = ImageSuite.image().symbols().INITIALIZE;
        STExecutableObject method = findMethod(initMethodName);
        if(method != null) {
            SchedulingSuite.callExecutableInNewProcess(method, this);
        }
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
        ImageSuite.image().put(className, klass);
        return klass;
    }
    
    
    public static STMetaclass create() {
        STMetaclass meta = new STMetaclass();

        meta.setClassProvider(new BindingClassProvider(meta) {
            
            private static final long serialVersionUID = 1L;

            @Override
            protected STClass _getSTClass() {
                return ImageSuite.image().classes().Metaclass;
            }
        });

        return meta;
    }
    
    public String toString() {
        return "<STMetaclass" + ((mInstanceClass != null) ? " for " + mInstanceClass.toString() : "") + ">";
    }
}
